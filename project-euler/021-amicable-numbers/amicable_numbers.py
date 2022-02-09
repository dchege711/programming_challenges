# Why is this much faster than sum_of_divisors?
def sum_of_divisors_manual(n):
    s = 0
    for i in range(1, n):
        if n % i == 0: s += i
    return s

def sum_of_divisors(n):
    def is_factor(f): return n % f == 0
    return sum(filter(is_factor, range(1, n)))

def is_amicable_number(n):
    s = sum_of_divisors_manual(n)
    if s == n: return False
    return sum_of_divisors_manual(s) == n

def sum_of_amicable_numbers(n):
    return sum(filter(is_amicable_number, range(1, n)))

if __name__ == "__main__":
    print(sum_of_amicable_numbers(10000))
