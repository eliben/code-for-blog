##
#
# Some useful utilities
# 
# by Eli Bendersky. License: LGPL
#
##
#


module Enumerable
    # Map a method call for each element.
    # For example: ar.mapf(:to_i)
    #
    def mapf(message)
        self.map {|elt| elt.send(message)}
    end
    
    # Return a random element from the enumeration
    #
    def rand
        at(Kernel.rand(self.size))
    end
    
    alias includes? include?
    alias contains? include?
end
