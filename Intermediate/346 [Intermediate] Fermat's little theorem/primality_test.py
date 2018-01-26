import sys 
sys.path.insert(0, "/Users/dchege711/utilities/")

import random
from test_utilities import test_utilities

class primality_test():
    def __init__(self, method=None):
        self.available_methods = {
            "fermats_little_theorem"
        }
    
    def is_prime(self, possible_prime, required_accuracy, 
        method="fermats_little_theorem"):
        if method not in self.available_methods:
            raise ValueError(
                "%s isn't available. Try %s" %(method, self.available_methods)
            )
        if method == "fermats_little_theorem":
            return self._run_fermats_test(possible_prime, required_accuracy)
        
    def _run_fermats_test(self, possible_prime, required_accuracy):
        random_number = random.randint(1, possible_prime-1)
        # pow(random_number, possible_prime) % possible_prime is inefficient
        remainder = pow(random_number, possible_prime, possible_prime)
        if remainder != random_number:
            return False, 1
        else:
            return True, 1
    
def main():
    fermats_test = primality_test()
    test_utility = test_utilities()
    with open(sys.argv[1], "r") as test_file:
        for line in test_file:
            possible_prime, required_accuracy, expected_answer = line.split()
            possible_prime = int(possible_prime)
            required_accuracy = float(required_accuracy)
            answer = str(fermats_test.is_prime(possible_prime, required_accuracy))
            test_utility.check(possible_prime, expected_answer, answer)
    print(test_utility.summary())
    
if __name__ == "__main__":
    main()
