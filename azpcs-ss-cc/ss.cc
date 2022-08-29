#include <algorithm>
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

	void Place(int x, int y) {
		auto value = Value(ps_.size());
		ps_.push_back({x,y});
		ns_[{x,y}] = -value;
		nps_[value].erase({x,y});
		for (int dx = -1; dx < 2; dx++) {
			const auto nx = x + dx;
			for (int dy = -1; dy < 2; dy++) {
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
			for (int x = -max_delta; x <= max_delta; x++) {
				for (int y = -max_delta; y <= max_delta; y++) {
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

	int Value(int n) const {
		return n < n_ ? 1 : (n - n_ + 2);
	}

	void Draw(bool full=true) const {
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

		if (full) {
			for (int i = 0; i < dx; i++) {
				for (int j = 0; j < dy; j++) {
					// C++20 required for fmt
					printf("%*d ", width, matrix[i * dy + j]);
				}
				printf("\n");
			}
		} else {
			for (int i = 0; i < dx; i++) {
				for (int j = 0; j < dy; j++) {
					if (matrix[i * dy + j] > 0) {
						printf("%*d ", width, matrix[i * dy + j]);
					} else {
						printf("%*c ", width, ' ');
					}
				}
				printf("\n");
			}
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

	void Draw() { s_.Draw(false); }

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

int main() {
#if 0
	TestState();
	TestChromo();
#else
	const int n = 7;
	const int pop_size = 100;
	const int md = 10;
	const float mutation_probability = 0.05;
	const int every_iterations = 10000;

	struct {
		bool operator()(Chromo a, Chromo b) const {
			return a.Fitness() > b.Fitness();
		}
	} FitnessSort;

	RNG gen;
	std::random_device r;
	gen.seed(r());

	std::vector<Chromo> population;
	for (int i = 0; i < pop_size; i++) {
		population.push_back(Chromo(&gen, n, md));
		population[i].Fitness();
	}
	std::sort(population.begin(), population.end(), FitnessSort);
	int best_fitness = population[0].Fitness();

	int num_iterations = 0;
	while (true) {
		++num_iterations;
		if (num_iterations % every_iterations == 0) {
			std::cout << "Iteration " << num_iterations << " best fitness " << best_fitness - n + 2 << "\n";
		}
		for (int i = 0; i < pop_size; i += 2) {
			population[i].Crossover(population[i+1]);
		}
		for (auto& c : population) {
			std::uniform_real_distribution<> u(0, 1);
			if (u(gen) < mutation_probability) {
				c.Mutate();
			}
			c.NextGen();
		}
		// NOTE: why does this need a separate loop?
		for (auto& c : population) {
			c.Fitness();
		}
		std::sort(population.begin(), population.end(), FitnessSort);
		if (population[0].Fitness() > best_fitness) {
			best_fitness = population[0].Fitness();
			std::cout << "New best fitness " << best_fitness - n + 2 << " from:\n";
			population[0].Draw();
		}
	}
#endif
	return 0;
}
