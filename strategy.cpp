#include <vector>
#include <iostream>
#include <functional>

using namespace std;

template <typename T> struct QuickSort
{
    static vector<T> sort(vector<T> const& input) { return input; }
};

template <typename T> struct MergeSort
{
    static vector<T> sort(vector<T> const& input) { return input; }
};

template <template<typename> class SortStrategy, typename T>
typename enable_if <
  is_same<SortStrategy<T>, QuickSort<T>>::value ||
  is_same<SortStrategy<T>, MergeSort<T>>::value ,
  bool>::type
unique ( vector<T> const& input )
{
    if (input.empty()) {
        return true;
    }
    auto sorted = SortStrategy<T>::sort(input);
    for (size_t i = 1; i < sorted.size(); ++i) {
        if (sorted[i - 1] == sorted[i]) {
            return false;
        }
    }
    return true;
}

template <typename T> struct MySort
{
    static vector<T> sort(vector<T> const& input) { return input; }
};

bool uniqueRT(vector<int> const& input,
            function<vector<int>(vector<int> const&)> sort)
{
    if (input.empty()) {
        return true;
    }
    vector<int> sorted = sort(input);
    for (size_t i = 1; i < sorted.size(); ++i) {
        if (sorted[i - 1] == sorted[i]) {
            return false;
        }
    }
    return true;
}

int main(void)
{
    vector<int> input { 1, 2, 3 };
    cout << unique<QuickSort, int>({1,2,3}) << endl;
    return 0;
}