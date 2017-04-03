# Merge sort.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

def merge_sort(lst):
    n = len(lst)
    if n <= 1:
        return lst
    else:
        mid = int(n / 2)
        return merge(merge_sort(lst[:mid]), merge_sort(lst[mid:]))


def merge(lst1, lst2):
    """Merges two sorted lists into a single sorted list.

    Returns new list. lst1 and lst2 are destroyed in the process."""
    result = []
    while lst1 or lst2:
        if not lst1:
            return result + lst2
        elif not lst2:
            return result + lst1
        if lst1[0] < lst2[0]:
            # Note: pop(0) may be slow -- this isn't optimized code.
            result.append(lst1.pop(0))
        else:
            result.append(lst2.pop(0))
    return result


def test_sorter(sortfunc):
    assert sortfunc([1]) == [1]
    assert sortfunc([]) == []
    assert sortfunc([1, 2]) == [1, 2]
    assert sortfunc([3, 2]) == [2, 3]
    assert sortfunc([4, 3, 2]) == [2, 3, 4]
    assert sortfunc([1, 3, 2]) == [1, 2, 3]
    assert sortfunc([7, 1, 3, 2]) == [1, 2, 3, 7]
    assert sortfunc([7, 1, 5, 3, 2]) == [1, 2, 3, 5, 7]
    assert sortfunc([9, 8, 7, 6, 5, 4, 3, 2, 1, 0]) == list(range(10))
    assert sortfunc([1, 8, 7, 6, 5, 4, 3, 2, 9, 0]) == list(range(10))
    assert sortfunc([4, 3, 7, 6, 5, 0, 1, 2, 9, 8]) == list(range(10))


def merge_sort_cps(lst, cont):
    n = len(lst)
    if n <= 1:
        return cont(lst)
    else:
        mid = int(n / 2)
        return merge_sort_cps(
                lst[:mid],
                lambda v1: merge_sort_cps(lst[mid:],
                                          lambda v2: cont(merge(v1, v2))))


if __name__ == '__main__':
    test_sorter(merge_sort)
    test_sorter(lambda lst: merge_sort_cps(lst, lambda value: value))
