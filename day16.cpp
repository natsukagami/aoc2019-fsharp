#include <iostream>
#include <cmath>
#include <string>

const int sz = 7e6;

int n;
int input[sz], sum[sz];

void perform() {
	for (int i = 1; i <= n; ++i) sum[i] = sum[i - 1] + input[i];
	for (int r = 1; r <= n; ++r) {
		int i = r - 1, sign = 1;
		int s = 0;
		while (i <= n) {
			s += (sum[std::min(n, i + r)] - sum[i]) * sign;
			i += 2 * r; sign = -sign;
		}
		input[r] = std::abs(s) % 10;
	}
}

std::string t;

int main() {
	std::cin >> t;
	n = t.length();
	for (int i = 0; i < n; ++i) {
		for (int j = 0, _j = 0; _j < 10000; ++_j, j += n) {
			input[i + j + 1] = t[i] - '0';
		}
	}
	int skip = 0;
	for (int i = 1; i <= 7; ++i) skip = skip * 10 + input[i];
	for (int i = 0; i < 100; ++i) {
		// std::cout << i << std::endl;
		perform();
		for (int j = 0; j < 100; ++j) std::cout << input[j + 1]; std::cout << std::endl;
	}
}
