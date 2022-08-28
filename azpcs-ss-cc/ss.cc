#include <iostream>
#include <map>
#include <set>
#include <utility>
#include <vector>

using Coord = std::pair<int,int>;

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
		ps_.emplace_back(x,y);
		ns_[{x,y}] = -value;
		nps_[value].erase({x,y});
		for (int dx = -1; dx < 2; dx++) {
			const auto nx = x + dx;
			for (int dy = -1; dy < 2; dy++) {
				const auto ny = y + dy;
				if (ns_[{nx,ny}] >= 0) {
					const auto old_value = ns_[{nx,ny}];
					nps_[old_value].erase({nx, ny});
					ns_[{nx,ny}] += value;
					const auto new_value = ns_[{nx,ny}];
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
				ret.emplace_back(x,y);
			}
		} else {
			for (int x = -max_delta; x <= max_delta; x++) {
				for (int y = -max_delta; y <= max_delta; y++) {
					if (ns_[{x,y}] >= 0) {
						ret.emplace_back(x, y);
					}
				}
			}
		}
		return ret;
	}

	int Value(int n) const {
		return n < n_ ? 1 : (n - n_ + 2);
	}

	void Draw() const {
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

		for (int i = 0; i < dx; i++) {
			for (int j = 0; j < dy; j++) {
				// C++20 required for fmt
				printf("%*d ", width, matrix[i * dy + j]);
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

int main() {
	TestState();
	return 0;
}
