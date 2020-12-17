#include <bits/stdc++.h>
using namespace std;
int main() {
    long long a;
    cin >> a;
    vector<int> arr(a);
    for (int i = 0; i < a; i++) {
	cin >> arr[i];
    }
    cout << *min_element(arr.begin(), arr.end());
}
