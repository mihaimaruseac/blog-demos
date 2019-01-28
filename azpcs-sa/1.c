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

static inline void compute_initial_distances()
{
	for (int i = 0; i < N2; i++)
		for (int j = 0; j < N2; j++)
			d[i][j] = dist(i, j);
}

static inline unsigned long compute_score(const int x[])
{
	unsigned long s = 0;
	unsigned long contrib;
	for (int i = 0; i < N2; i++)
		for (int j = i + 1; j < N2; j++) {
			contrib = d[i][j] * dist(x[i], x[j]);
			s += contrib;
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

int main()
{
	init_rng();
	compute_initial_distances();
	build_initial_state();

	unsigned int score = compute_score(state);
	if (score < best_score) {
		best_score = score;
		copy_to(state, best);
		printf("Best score: %ld\n", best_score);
	}
	short_print(best);

	return 0;
}
