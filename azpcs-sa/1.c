#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define N 3
#define N2 (N * N)
#define MIN_BOUND 72

#define OO 99999999999L

static const char letters[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234";
static unsigned long d[N2][N2];
static int state[N2];
static unsigned long best_score = OO;
static int best[N2];
static int ci, cj;

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

static inline void short_print(const int x[])
{
	for (int i = 0; i < N2; i++)
		printf("%d ", x[i]);
	printf("\n");
}

static inline unsigned long dist(int i, int j)
{
	unsigned long dx = abs((i % N) - (j % N));
	unsigned long dy = abs((i / N) - (j / N));
	if (dx > N - dx) dx = N - dx;
	if (dy > N - dy) dy = N - dy;
	return dx * dx + dy * dy;
}

static inline void compute_initial_distances(void)
{
	for (int i = 0; i < N2; i++)
		for (int j = 0; j < N2; j++)
			d[i][j] = dist(i, j);
}

static inline unsigned long compute_score(const int x[], int update_contrib)
{
	unsigned long s = 0;
	unsigned long contrib;
	unsigned long largest_contrib = 0;

	for (int i = 0; i < N2; i++)
		for (int j = i + 1; j < N2; j++) {
			contrib = d[i][j] * dist(x[i], x[j]);
			s += contrib;

			if (contrib > largest_contrib && update_contrib) {
				largest_contrib = contrib;
				ci = i;
				cj = j;
			}
		}
	return s - MIN_BOUND;
}

static inline void init_rng(void)
{
	struct timeval now;
	gettimeofday(&now, NULL);
	long int seedval = now.tv_usec + now.tv_sec;
	seedval = 42;
	printf("Using seed %ld\n", seedval);
	srand48(seedval);
}

static inline void build_initial_state(void)
{
	for (int i = 0; i < N2; i++)
		state[i] = i;
}

static inline void copy_to(int src[], int dst[])
{
	for (int i = 0; i < N2; i++)
		dst[i] = src[i];
}

static inline void swap(int x[], int ix1, int ix2)
{
	int t = x[ix1];
	x[ix1] = x[ix2];
	x[ix2] = t;
}

static inline void try_swap(int x[], int ix1, int ix2, unsigned long score_now)
{
	unsigned long best_scores[2] = {OO, OO};
	int best_ixs[2] = {ix1, ix2};
	int ixs[2] = {ix1, ix2};
	unsigned int score;

	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < N2; j++) {
			if (j == ixs[i])
				continue;
			swap(x, j, ixs[i]);
			score = compute_score(x, /*update_contrib=*/0);
			swap(x, j, ixs[i]);

			if (score < best_scores[i]) {
				best_scores[i] = score;
				best_ixs[i] = j;
			}
		}
	}

	unsigned long best_score_here = best_scores[0];
	unsigned long moving_ix1 = best_ixs[0];
	unsigned long moving_ix2 = ixs[0];
	if (best_score_here > best_scores[1]) {
		best_score_here = best_scores[1];
		moving_ix1 = best_ixs[1];
		moving_ix2 = ixs[1];
	}

	// TODO: annealing
	if (best_score_here < score_now)
		swap(x, moving_ix1, moving_ix2);
}

int main()
{
	init_rng();
	compute_initial_distances();
	build_initial_state();

	unsigned int score = compute_score(state, /*update_contrib=*/1);
	if (score < best_score) {
		best_score = score;
		copy_to(state, best);
		printf("Best score: %ld\n", best_score);
		print_grid(best);
		printf("\n");
	}
	short_print(state);
	printf("Largest contrib bw. %d %d\n", ci, cj);
	try_swap(state, ci, cj, score);
	score = compute_score(state, /*update_contrib=*/0);
	printf("%d\n", score);
	short_print(state);

	return 0;
}
