#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define N 3 //5
#define N2 (N * N)
#define MIN_BOUND 72 //3800
#define POPSZ 5 //100
#define FAMSZ 3

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
	srand48(41);//now.tv_usec + now.tv_sec);
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
	
	// debug; print array fast
	for (int i = 0; i < N2; i++) printf("%d ", x[i]);
	printf("\n");
}

static inline void initialize_population(void)
{
	for (int i = 0; i < POPSZ; i++)
		shuffle(pop1[i], N2);

	// debug print
	for (int i = 0; i < POPSZ; i++) {
		printf("Pop %d: ", i);
		for (int j = 0; j < N2; j++) {
			printf("%d ", pop1[i][j]);
		}
		printf("\n");
	}
}

static inline void copy_to(int src[], int dst[])
{
	for (int i = 0; i < N2; i++)
		dst[i] = src[i];
}

static inline void compute_scores()
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
	}

	// debug
	printf("Scores: ");
	for (int i = 0; i < POPSZ; i++)
		printf("%lu ", scores[i]);
	printf("\nBest: %lu (%d)\n", best_now, best_ix);
	printf("At generation %d best score is %lu for: ", epoch, best_score);
	for (int i = 0; i < N2; i++)
		printf("%d ", best[i]);
	printf("\n");
}

int main()
{
	init_rng();
	compute_initial_distances();
	initialize_population();

	compute_scores();

	return 0;
}
