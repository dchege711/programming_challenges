import random

class fermats_primality_test():
    def __init__(self):
        pass 
    
    def is_prime(self, number, required_accuracy):
        verdict, current_accuracy = True, 0
        while (current_accuracy < required_accuracy):
            verdict, current_accuracy = self._run_fermats_test(number)
        return verdict
        
    def _run_fermats_test(self, number, confidence):
        random_number = random.randint(1, number-1)
        remainder = (a ** number) % number
        if remainder != random_number:
            return False, 1
        else:
            return True, 0.8
    
def main():
    
    
    
