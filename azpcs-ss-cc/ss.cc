#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <map>
#include <random>
#include <set>
#include <utility>
#include <vector>

using Coord = std::pair<int,int>;
using RNG = std::mt19937;

int NumDigits(int n) {
	int v = 0;
	while (n > 0) {
		v++;
		n/=10;
	}
	return v;
}

namespace { // State

class State {
  public:
	State(int n) : n_(n) {
		Place(0, 0);
	}

	State(const State &other) : ps_(other.ps_), ns_(other.ns_), nps_(other.nps_), n_(other.n_) {}

	void Place(int x, int y) {
		auto value = Value(ps_.size());
		ps_.push_back({x,y});
		ns_[{x,y}] = -value;
		nps_[value].erase({x,y});
		for (int dx = -1; dx < 2; ++dx) {
			const auto nx = x + dx;
			for (int dy = -1; dy < 2; ++dy) {
				const auto ny = y + dy;
				if (ns_[{nx,ny}] >= 0) {
					const auto old_value = ns_[{nx,ny}];
					const auto new_value = old_value + value;
					ns_[{nx,ny}] = new_value;
					nps_[old_value].erase({nx, ny});
					nps_[new_value].insert({nx, ny});
				}
			}
		}
	}

	std::vector<Coord> Next(int max_delta = 10) {
		const auto value = Value(ps_.size());
		std::vector<Coord> ret;
		if (value > 1) {
			for (const auto& [x,y]: nps_[value]) {
				ret.push_back({x,y});
			}
		} else {
			for (int x = -max_delta; x <= max_delta; ++x) {
				for (int y = -max_delta; y <= max_delta; ++y) {
					const auto& key = Coord{x,y};
					const auto& found = ns_.find(key);
					// if not found or if found but positive
					if (found == ns_.end() || found->second >= 0) {
						ret.push_back(key);
					}
				}
			}
		}
		return ret;
	}

	int Score() const {
		return ps_.size();
	}

	int Value(int n) const {
		return n < n_ ? 1 : (n - n_ + 2);
	}

	void Draw(bool space=false, bool input=false) const {
		int xmin = 0, ymin = 0, xmax = 0, ymax = 0;
		for (const auto& [c,_]: ns_) {
			const auto& [x,y] = c;
			if (xmin > x) xmin = x;
			if (ymin > y) ymin = y;
			if (xmax < x) xmax = x;
			if (ymax < y) ymax = y;
		}
		const auto dx = xmax - xmin + 1;
		const auto dy = ymax - ymin + 1;
		const auto N = dx * dy;
		// No 0-init in pure C++
		auto matrix = static_cast<int*>(calloc(N, sizeof(int)));

		int max_v = 0;
		for (const auto& [c,v]: ns_) {
			const auto& [x,y] = c;
			matrix[(x - xmin) * dy + (y - ymin)] = -v;
			if (max_v < v) max_v = v;
		}
		const int width = 1 + NumDigits(max_v);

		for (int i = 0; i < dx; ++i) {
			for (int j = 0; j < dy; ++j) {
				// C++20 required for fmt
				printf("%*d ", width, matrix[i * dy + j]);
			}
			printf("\n");
		}
		if (space) {
			for (int i = 0; i < dx; ++i) {
				for (int j = 0; j < dy; ++j) {
					if (matrix[i * dy + j] > 0) {
						printf("%*d ", width, matrix[i * dy + j]);
					} else {
						printf("%*c ", width, ' ');
					}
				}
				printf("\n");
			}
		}
		if (input) {
			for (int i = 1; i < dx-1; ++i) {
				int space = 0;
				bool first = true;
				printf("(");
				for (int j = 1; j < dy; ++j) {
					if (matrix[i * dy + j] > 0) {
						if (first) {
							first = false;
						} else {
							printf(",");
						}
						if (space > 0) {
							printf("%d,%d", -space, matrix[i * dy + j]);
						} else {
							printf("%d", matrix[i * dy + j]);
						}
						space = 0;
					} else {
						space++;
					}
				}
				printf("),");
			}
			printf("\n");
		}

		free(matrix);
	}

	void Debug() const {
		std::cout << "n_=" << n_ << ", next=" << Value(ps_.size())
			  << ", placed: " << ps_.size() << "\n";
		std::cout << "ps_: ";
		for (const auto& [x,y]: ps_)
			std::cout << "{" << x << "," << y << "} ";
		std::cout << "\n";
		std::cout << "ns_: ";
		for (const auto& [p,v]: ns_) {
			if (!v) continue;
			const auto& [x,y] = p;
			std::cout << "{<" << x << "," << y << ">:" << v << "} ";
		}
		std::cout << "\n";
		std::cout << "nps_:\n";
		for (const auto& [k,v]: nps_) {
			std::cout << "\t" << k << ":";
			for (const auto& [x,y] : v) {
				std::cout << " {" << x << "," << y << "}";
			}
			std::cout << "\n";
		}
	}

  private:
	std::vector<Coord> ps_;
	std::map<Coord, int> ns_;
	std::map<int, std::set<Coord>> nps_;
	int n_;
};

void TestEmpty() {
	std::cout << "============ TestEmpty ============\n";
	State s(2);
	s.Draw();
}

void TestPlaceNear() {
	std::cout << "========== TestPlaceNear ==========\n";
	State s(2);
	s.Place(0, 1);
	s.Draw();
}

void TestPlaceNext() {
	std::cout << "========== TestPlaceNext ==========\n";
	State s(2);
	s.Place(0, 1);
	s.Place(1, 1);
	s.Draw();
}

void TestPlaceNext2() {
	std::cout << "========== TestPlaceNext2 =========\n";
	State s(2);
	s.Place(0, 1);
	s.Place(1, 1);
	s.Place(0, 2);
	s.Draw();
}

void TestGetNextPos() {
	std::cout << "========== TestGetNextPos =========\n";
	State s(2);
	s.Place(0, 1);
	s.Place(1, 1);
	s.Place(0, 2);
	for (const auto& [x,y] : s.Next()) {
		std::cout << "{" << x << "," << y << "} ";
	}
	std::cout << "\n";
}

void TestGetNextPos2() {
	std::cout << "========= TestGetNextPos2 =========\n";
	State s(2);
	for (const auto& [x,y] : s.Next(1)) {
		std::cout << "{" << x << "," << y << "} ";
	}
	std::cout << "\n";
}

void TestPlaceNext3() {
	std::cout << "========== TestPlaceNext3 =========\n";
	State s(3);
	while (true) {
		const auto next = s.Next(2);
		if (!next.size()) break;
		const auto& [x,y] = next[0];
		s.Place(x, y);
	}
	s.Draw();
}

void TestPlaceNext4() {
	std::cout << "========== TestPlaceNext4 =========\n";
	State s(3);
	while (true) {
		const auto next = s.Next(2);
		if (!next.size()) break;
		const auto& [x,y] = next[next.size() / 2];
		s.Place(x, y);
	}
	s.Draw();
}

void TestState() {
	TestEmpty();
	TestPlaceNear();
	TestPlaceNext();
	TestPlaceNext2();
	TestGetNextPos();
	TestGetNextPos2();
	TestPlaceNext3();
	TestPlaceNext4();
}

} // namespace // State

namespace { // Chromo

class Chromo {
  public:
	Chromo(RNG* rng, int n, int md=10) : fitness_(0), n_(n), rng_(rng), md_(md), s_(n_) {}

	Chromo(const Chromo &other) : genes_(other.genes_), fitness_(0), n_(other.n_), rng_(other.rng_), md_(other.md_), s_(other.n_) {}

	Chromo& operator=(const Chromo &other) {
		if (this == &other)
			return *this;

		genes_.clear();
		genes_ = std::vector<int>(other.genes_);
		fitness_ = 0;
		n_ = other.n_;
		rng_ = other.rng_;
		md_ = other.md_;
		s_ = other.s_;
		return *this;
	}

	State Grow(int reps) {
		State s(n_);
		State best(n_);
		for (const auto& g: genes_) { // debug only, mostly unused
			const auto& next = s.Next(md_);
			if (!next.size()) return best; // safety measure, impossible
			const auto& [x,y] = next[g % next.size()];
			s.Place(x, y);
		}
		while (true) {
			const auto& next = s.Next(md_);
			if (!next.size()) break;
			auto best_x = 0, best_y = 0; // surely overriden
			auto best_of_n = s.Score();
			for (auto try_ = 0; try_ < reps; ++try_) {
				auto best_score = s.Score();
				auto best_next = -1;
				for (unsigned int ix = 0; ix < next.size(); ++ix) {
					const auto& [x,y] = next[ix];
					State ss(s);
					ss.Place(x, y);
					while (true) {
						const unsigned int g = (*rng_)();
						const auto& next = ss.Next(md_);
						if (!next.size()) break;
						const auto& [x,y] = next[g % next.size()];
						ss.Place(x, y);
					}
					auto now_score = ss.Score();
					if (now_score > best_score) {
						best_score = now_score;
						best_next = ix;
						if (now_score > best.Score()) {
							best = ss;
							best.Draw(true, true);
						}
					}
				}
				if (best_next < 0) break; // impossible
				if (best_score > best_of_n) {
					const auto& [x,y] = next[best_next];
					best_x = x;
					best_y = y;
					best_of_n = best_score;
				}
			}
			s.Place(best_x, best_y);
		}
		return best;
	}

	int Fitness() {
		if (fitness_) return fitness_;
		State s(n_);
		bool shorter = false;
		for (const auto& g: genes_) {
			const auto& next = s.Next(md_);
			if (!next.size()) {
				shorter = true;
				break;
			}
			const auto& [x,y] = next[g % next.size()];
			s.Place(x, y);
			++fitness_;
		}
		if (shorter) {
			genes_.resize(fitness_);
		} else {
			while (true) {
				const unsigned int g = (*rng_)();
				const auto& next = s.Next(md_);
				if (!next.size()) break;
				const auto& [x,y] = next[g % next.size()];
				s.Place(x, y);
				genes_.push_back(g);
				++fitness_;
			}
		}
		s_ = s;
		return fitness_;
	}

	void Crossover(Chromo& other) {
		std::uniform_int_distribution<> distrib(0,
				std::min(genes_.size(), other.genes_.size()) - 1);
		int ix = distrib(*rng_);
		auto ti = genes_.begin() + ix;
		auto oi = other.genes_.begin() + ix;
		for(; ti != genes_.end() && oi != other.genes_.end(); ++ti, ++oi) {
			std::swap(*ti, *oi);
		}
	}

	void Mutate() {
		std::uniform_int_distribution<> distrib(0, genes_.size() - 1);
		int ix = distrib(*rng_);
		if (ix < n_) {
			std::uniform_int_distribution<> d(0, 100);
			genes_[ix] += distrib(*rng_);
		} else {
			++genes_[ix];
		}
	}

	void NextGen() { fitness_ = 0; }

	void Draw(bool space=false, bool input=false) { s_.Draw(space, input); }

	// must be called after Fitness()
	int Size() const { return genes_.size(); }

	void Bump(int ix, bool shrt) {
		fitness_ = 0;
		if (shrt) {
			++genes_[ix];
		} else {
			std::uniform_int_distribution<> d(2, md_ * md_ - 1);
			genes_[ix] += d(*rng_);
		}
	}

	void AddGene(int x) { genes_.push_back(x); } // testing only

	void Dump() const { // testing only
		std::cout << "{";
		for (const auto& g : genes_) std::cout << g << " ";
		std::cout << "}\n";
	}

  private:
	std::vector<int> genes_;
	int fitness_;
	int n_;
	RNG* rng_; // not owned
	int md_;
	State s_; // only computed in Fitness()
};

void TestInitialFitness() {
	std::cout << "======= TestInitialFitness ========\n";
	RNG gen;
	gen.seed(42);
	Chromo c(&gen, 3);
	std::cout << c.Fitness() << " " << c.Fitness() << "\n";
}

void TestGivenFitness() {
	std::cout << "======== TestGivenFitness =========\n";
	RNG gen;
	gen.seed(42);
	Chromo c(&gen, 3);
	c.AddGene(200);
	std::cout << c.Fitness() << "\n";
}

void TestGivenFitness2() {
	std::cout << "======== TestGivenFitness2 ========\n";
	RNG gen;
	gen.seed(42);
	Chromo c(&gen, 3);
	for (const auto& g : {200, 200, 3, 3, 0,0,0,0,0,0, 42}) {
		c.AddGene(g);
	}
	std::cout << c.Fitness() << "\n";
	c.Dump();
}

void TestCrossover() {
	std::cout << "========== TestCrossover ==========\n";
	RNG gen;
	gen.seed(42);
	Chromo c1(&gen, 3);
	for (const auto& g : {1,2,3,4,5,6,7}) {
		c1.AddGene(g);
	}
	Chromo c2(&gen, 3);
	for (const auto& g : {11,12,13,14,15,16,17,18,19,20}) {
		c2.AddGene(g);
	}
	c1.Dump();
	c2.Dump();
	c1.Crossover(c2);
	c1.Dump();
	c2.Dump();
}

void TestMutate() {
	std::cout << "=========== TestMutate ============\n";
	RNG gen;
	gen.seed(42);
	Chromo c(&gen, 3);
	for (const auto& g : {200, 200, 3, 3, 0,0,0,0,0,0, 42}) {
		c.AddGene(g);
	}
	c.Dump();
	c.Mutate();
	c.Dump();
}

void TestChromo() {
	TestInitialFitness();
	TestGivenFitness();
	TestGivenFitness2();
	TestCrossover();
	TestMutate();
}

} // namespace // Chromo

std::tuple<int, int, int> parseArgs(int argc, char **argv) {
	if (argc >= 3) return {std::atoi(argv[1]), std::atoi(argv[2]), std::atoi(argv[3])};
	if (argc >= 2) return {std::atoi(argv[1]), std::atoi(argv[2]), 10};
	if (argc >= 1) return {std::atoi(argv[1]), 10, 10};
	return {31, 10, 10};
}

int main(int argc, char **argv) {
	const auto& [n, md, reps] = parseArgs(argc, argv);
	if (n < 2 || md < 2 || reps < 3) return 0;

	RNG gen;
	std::random_device r;
	gen.seed(r());

	Chromo c(&gen, n, md);
	State s = c.Grow(reps);
	s.Draw(true, true);
	std::cout << s.Score() << "\n";

	if (false) {
		TestState();
		TestChromo();
	}
	return 0;
}
