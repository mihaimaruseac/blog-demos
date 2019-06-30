#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define N 6
#define N2 (N * N)
#define MIN_BOUND 16902
#define POPSZ 100
#define FAMSZ 5
#define MAX_EPOCH 100000000
#define MUTATION_PROBABILITY 0.1

#define OO 99999999999L

static const char letters[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234";
static unsigned long d[N2][N2];

static int pop1[POPSZ][N2];
static int pop2[POPSZ][N2];
static unsigned long scores[POPSZ];
static int epoch = 0;
static unsigned long best_score = OO;
static int best[N2];

static inline void print_one(int x)
{
	printf("%c%c", letters[x % N], letters[x / N]);
}

static inline void print_grid(const int x[])
{
	int i = 0, j;
	goto start;

	do {
		printf("),\n");
start:
		printf("("); print_one(x[i++]);
		for (j = 1; j < N; j++) {
			printf(", ");
			print_one(x[i++]);
		}
	} while (i < N2);
	printf(")\n");
}

static inline unsigned long dist(int i, int j)
{
	unsigned long dx = abs((i % N) - (j % N));
	unsigned long dy = abs((i / N) - (j / N));
	if (dx > N - dx) dx = N - dx;
	if (dy > N - dy) dy = N - dy;
	return dx * dx + dy * dy;
}

static inline void compute_initial_distances()
{
	for (int i = 0; i < N2; i++)
		for (int j = 0; j < N2; j++)
			d[i][j] = dist(i, j);
}

static inline unsigned long compute_score(const int x[])
{
	unsigned long s = 0;
	for (int i = 0; i < N2; i++)
		for (int j = i + 1; j < N2; j++)
			s += d[i][j] * dist(x[i], x[j]);
	return s - MIN_BOUND;
}

static inline void init_rng(void)
{
	struct timeval now;
	gettimeofday(&now, NULL);
	long int seedval = now.tv_usec + now.tv_sec;
	printf("Using seed %ld\n", seedval);
	srand48(seedval);
}

static inline void shuffle(int x[], int n)
{
	/* first write array in order */
	for (int i = 0; i < n; i++) x[i] = i;

	/* now do a Fisher-Yates shuffle of x */
	for (int i = n - 1; i > 0; i--) {
		int j = drand48() * i;
		int t = x[i];
		x[i] = x[j];
		x[j] = t;
	}
}

static inline void initialize_population(void)
{
	for (int i = 0; i < POPSZ; i++)
		shuffle(pop1[i], N2);
}

static inline void copy_to(int src[], int dst[])
{
	for (int i = 0; i < N2; i++)
		dst[i] = src[i];
}

static inline void compute_scores(void)
{
	if (epoch % 2 == 0) {
		for (int i = 0; i < POPSZ; i++)
			scores[i] = compute_score(pop1[i]);
	} else {
		for (int i = 0; i < POPSZ; i++)
			scores[i] = compute_score(pop2[i]);
	}

	unsigned long best_now = scores[0];
	int best_ix = 0;
	for (int i  = 1; i < POPSZ; i++)
		if (best_now > scores[i]) {
			best_now = scores[i];
			best_ix = i;
		}

	if (best_score > best_now) {
		best_score = best_now;
		if (epoch % 2 == 0)
			copy_to(pop1[best_ix], best);
		else
			copy_to(pop2[best_ix], best);

		/* also print best score */
		printf("Next best score: %ld (epoch: %d)\n", best_score, epoch);
		print_grid(best);
		printf("\n");
	}
}

static inline void combine(int son_ix, int parent1_ix, int parent2_ix)
{
	/* crossover */
	int x1 = drand48() * (N2 - 2);
	int x2 = x1 + 1 + (drand48() * (N2 / 2));
	if (x2 > N2) x2 = N2;

	if (epoch % 2 == 0) {
		/* from pop1 to pop2 */
		for (int i = x1; i < x2; i++)
			pop2[son_ix][i] = pop1[parent1_ix][i];
		int i = 0, j = 0;
		while (i < x1) {
			int found = 0;
			for (int k = x1; k < x2; k++)
				if (pop2[son_ix][k] == pop1[parent2_ix][j]) {
					found = 1;
					break;
				}
			if (!found)
				pop2[son_ix][i++] = pop1[parent2_ix][j];
			j++;
		}
		i = x2;
		while (i < N2) {
			int found = 0;
			for (int k = x1; k < x2; k++)
				if (pop2[son_ix][k] == pop1[parent2_ix][j]) {
					found = 1;
					break;
				}
			if (!found)
				pop2[son_ix][i++] = pop1[parent2_ix][j];
			j++;
		}
	} else {
		/* from pop2 to pop1 */
		for (int i = x1; i < x2; i++)
			pop1[son_ix][i] = pop2[parent1_ix][i];
		int i = 0, j = 0;
		while (i < x1) {
			int found = 0;
			for (int k = x1; k < x2; k++)
				if (pop1[son_ix][k] == pop2[parent2_ix][j]) {
					found = 1;
					break;
				}
			if (!found)
				pop1[son_ix][i++] = pop2[parent2_ix][j];
			j++;
		}
		i = x2;
		while (i < N2) {
			int found = 0;
			for (int k = x1; k < x2; k++)
				if (pop1[son_ix][k] == pop2[parent2_ix][j]) {
					found = 1;
					break;
				}
			if (!found)
				pop1[son_ix][i++] = pop2[parent2_ix][j];
			j++;
		}
	}

	/* mutation */
	if (drand48() < MUTATION_PROBABILITY) {
		int ix = drand48() * (N2 - 1);
		if (epoch % 2 == 0) {
			int t = pop2[son_ix][ix];
			pop2[son_ix][ix] = pop2[son_ix][ix + 1];
			pop2[son_ix][ix + 1] = t;
		} else {
			int t = pop1[son_ix][ix];
			pop1[son_ix][ix] = pop1[son_ix][ix + 1];
			pop1[son_ix][ix + 1] = t;
		}
	}
}

static inline void next_generation(void)
{
	static int family[POPSZ];
	unsigned long best1, best2;
	int best1_ix, best2_ix;

	for (int i = 0; i < POPSZ; i++) {
		/* generate new group to reproduce from */
		shuffle(family, POPSZ);

		/* first parent */
		best1 = scores[family[0]];
		best1_ix = 0;
		for (int j = 1; j < FAMSZ; j++)
			if (scores[family[j]] < best1) {
				best1 = scores[family[j]];
				best1_ix = j;
			}

		/* second parent */
		best2_ix = best1_ix == 0 ? 1 : 0;
		best2 = scores[family[best2_ix]];
		for (int j = 1; j < FAMSZ; j++)
			if (j != best1_ix && scores[family[j]] < best2) {
				best2 = scores[family[j]];
				best2_ix = j;
			}

		combine(i, family[best1_ix], family[best2_ix]);
	}
	epoch++;
}

int main()
{
	init_rng();
	compute_initial_distances();
	initialize_population();

	while (epoch < MAX_EPOCH) {
		compute_scores();
		next_generation();
	}

	return 0;
}
