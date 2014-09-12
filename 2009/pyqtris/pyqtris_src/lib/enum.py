"""
    A function named Enum that creates enumeration types.
    
        Days = Enum('Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa', 'Su')
        
        monday = Days.Mo
        tuesday = Days.Tu
        
        print monday
        
        if monday == tuesday:
            print "monday == tuesday"
        
        print "All days: "
        print Days
        
    Kindly borrowed from a recipe submitted by Zoran Isailovski to the 
    ASPN Python Cookbook:
        http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/413486
    
    Some points to note (taken from the link above):
    
    Most propositions for an enum in python attempt to solve the issue 
    with a single class. However, fact is that enum has a dual nature: 
    It declares a new anonimous type *and* all possible instances (values) 
    of that type at the same time. In other words, there is a distinction 
    between an enum type and its associated values.

    In recognition of this fact, this recipe uses *two* python classes 
    and python's nested scopes to accomplish a clean and concise 
    implementation.

    Note that
        -   Enums are immutable; attributes cannot be added, deleted 
            or changed.
        -   Enums are iterable.
        -   Enum value access is symbolic and qualified, ex. Days.Monday
        -   Enum values are true constants.
        -   Enum values are comparable.
        -   Enum values are invertible (usefull for 2-valued enums, 
            like Enum('no', 'yes').
        -   Enum values are usable as truth values (in a C tradition, 
            but this is debatable).
        -   Enum values are reasonably introspecitve (by publishing their 
            enum type and numeric value)
"""

def Enum(*names):
    assert names, "Empty enums are not supported" # <- Don't like empty enums? Uncomment!

    class EnumClass(object):
        __slots__ = names
        def __iter__(self):         return iter(constants)
        def __len__(self):          return len(constants)
        def __getitem__(self, i):   return constants[i]
        def __repr__(self):         return 'Enum' + str(names)
        def __str__(self):          return 'enum ' + str(constants)

    class EnumValue(object):
        __slots__ = ('__value')
        def __init__(self, value): self.__value = value
            
        Value = property(lambda self: self.__value)
        EnumType = property(lambda self: EnumType)
        
        def __hash__(self):         return hash(self.__value)
        def __invert__(self):       return constants[maximum - self.__value]
        def __nonzero__(self):      return bool(self.__value)
        def __repr__(self):         return str(names[self.__value])

        def __cmp__(self, other):
            assert self.EnumType is other.EnumType, "Only values from the same enum are comparable"
            return cmp(self.__value, other.__value)
            
    maximum = len(names) - 1
    constants = [None] * len(names)
    
    for i, each in enumerate(names):
        val = EnumValue(i)
        setattr(EnumClass, each, val)
        constants[i] = val
        
    constants = tuple(constants)
    EnumType = EnumClass()
    return EnumType


if __name__ == '__main__':
    Days = Enum('Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa', 'Su')
    print Days
    print Days.Mo
    print Days[0]
    print Days.Mo < Days.Fr
    print list(Days)
    for each in Days:
        print 'Day:', each
    print '--- Yes/No ---'
    Confirmation = Enum('No', 'Yes')
    answer = Confirmation.No
    print 'Your answer is not', ~answer

    joe = Days.Mo

    if joe == Days.Mo:
        print "holla"
