#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 5
#define N2 (N * N)

const char letters[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234";
unsigned int d[N2][N2];

const int grid1[] = {20, 8, 10, 19, 2, 17, 16, 1, 12, 6, 0, 14, 18, 15, 21, 23, 3, 4, 13, 7, 9, 5, 24, 22, 11};
const int grid2[] = {12, 15, 24, 2, 18, 0, 20, 7, 17, 5, 22, 10, 8, 19, 9, 3, 23, 11, 21, 16, 1, 13, 6, 4, 14};

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

static inline unsigned int dist(int i, int j)
{
	unsigned int dx = abs((i % N) - (j % N));
	unsigned int dy = abs((i / N) - (j / N));
	if (dx > N - dx) dx = N - dx;
	if (dy > N - dy) dy = N - dy;
	return dx * dx + dy * dy;
}

static inline void compute_initial()
{
	for (int i = 0; i < N2; i++)
		for (int j = 0; j < N2; j++)
			d[i][j] = dist(i, j);
}

static inline unsigned int compute_score(const int x[])
{
	unsigned int s = 0;
	for (int i = 0; i < N2; i++)
		for (int j = i + 1; j < N2; j++)
			s += d[i][j] * dist(x[i], x[j]);
	return s;
}

int main()
{
	compute_initial();
	printf("Grid 1: %u\n", compute_score(grid1));
	printf("Grid 2: %u\n", compute_score(grid2));
	int init[N2];
	for (int i = 0; i < N2; i++) init[i] = i;
	printf("No change: %u\n", compute_score(init));

	return 0;
}
