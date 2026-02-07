#include <iostream>
#include <bits/stdc++.h> // Includes most standard libraries
using namespace std;

// Define common macros for convenience
#define ll long long
#define endl '\n' // Use '\n' instead of std::endl for faster output
#define fastio ios_base::sync_with_stdio(false); cin.tie(NULL); cout.tie(NULL);

// Main function where your problem-solving logic resides
void solve() {
    // Your code for solving the problem goes here
    // Example: reading input and printing output
    int n, k;
    cin >> n >> k;

	int r, a[n];
	ll s[n];
	s[0] = 0;

	for(r = 0; r < n; r++) {
		cin >> a[r];
		s[r+1] = s[r] + a[r];
	}

	ll max_so_far = 0;
	int l = 0;

	for(r = 0; r < n; r++) {
		if (r+1-l > k) { l++; }
		if (s[r+1] - s[l] <= 0) { l = r+1; }
		max_so_far = max(max_so_far, s[r+1] - s[l]);
	}

	cout << max_so_far << endl;
}

int main() {
    fastio; // Call fast I/O setup
    solve();
    return 0;
}
