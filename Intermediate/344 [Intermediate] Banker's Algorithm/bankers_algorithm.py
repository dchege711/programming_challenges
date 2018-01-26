import sys

class bankers_algorithm():
    def __init__(self, initial_resources=None):
        self.resources = initial_resources
        if initial_resources is not None:
            self.num_resources = len(initial_resources)
        
    def set_initial_resources(self, initial_resources):
        self.resources = initial_resources
        self.num_resources = len(initial_resources)
        
    def add_process(self, already_allocated, max_needed):
        len_already_allocated = len(already_allocated)
        len_max_needed = len(max_needed)
        assert len_already_allocated == len_max_needed, "{0:d} resources already allocated don't match {0:d} needed".format(
                len_already_allocated, len_max_needed
            )
        assert len_already_allocated == self.num_resources, "Process requires {0:d} resources but we have {0:d} available".format(
                len_already_allocated, self.num_resources
            )
        
        
        
    def order_processes(self):
        return None
    
def main():
    
    client = bankers_algorithm()
    
    with open(sys.argv[1], "r") as test_file:
        num_processes = int(test_file.readline())
        
        initial_resources = test_file.readline().split()
        initial_resources = [int(x) for x in initial_resources]
        
        client.set_initial_resources(initial_resources)
            
        for i in range(num_processes):
            process = test_file.readline().split(",")
            already_allocated = [int(x) for x in process[0].split()]
            max_needed = [int(x) for x in process[1].split()]
            client.add_process(already_allocated, max_needed)
    
    print(client.order_processes())
            
    
    
if __name__ == "__main__":
    main()
