""" FALL 2013 """

d = [ ("a", 10), ("b", 20), ("c", 30), ("a", 40) ]

def lookup(d,k):
  return [ val for (key,val) in d if k==key ]

def update(d,k,v):
  return [ (key,v) if key==k else (key,value) for (key,value) in d ]

#alternate, using cond() function
def update(d,k,v):
  return [ (key, cond(k==key,v,val)) for (key,val) in d ]

def delete(d,k):
  return [ (key,value) for (key,value) in d if key!=k]

def add(d,k,v):
  return d + [(k,v)]

def update(d,k,v):
  ret = []
  for (key, value) in d:
    if key==k:
      ret.append((k,v))
    else:
      ret.append((key,value))
  return ret

def in_range(i, range):
  def decorator(f):
    def decorated(*args):
      if i == -1:
        res = f(*args)
        if range[0] <= res <= range[1]:
          return res
        else:
          raise Exception("not in range")
      elif 0 <= i <len(args):
        if range[0] <= arg[i] <= range[1]:
          return f(*args)
        else:
          raise Exception("not in range")
      else:
        return f(*args)
    return decorated
  return decorator

#class implementation
def in_range(i, range):
  class decorator:
    def __init__(self, f):
      self.f = f
      self.__name__ = f.__name__
    def __call__(self, *args):
      if i < len(args):
        if i == -1:
          if self.f(*args) < range[0]:
            raise Exception("Return value " + str(self.f(*args)) + " too small")
          elif self.f(*args) > range[1]:
            raise Exception("Return value " + str(self.f(*args)) + " too big")
          else:
            return self.f(*args)
        else:
          if args[i] < range[0]:
            raise Exception(str(i)+ "th arg " + str(args[i]) + " too small")
          elif args[i] > range[1]:
            raise Exception(str(i)+"th arg " + str(args[i]) + " too big")
          else:
            return self.f(*args)
  return decorator

""" ------------------------------------------------- """
""" SPRING 2013 """

def rev(l):
  return [ l[len(l)-i-1] for i in range(len(l)) ]

#alternate
def rev(l):
  return [ x for x in l[::-1] ]

#example of using reduce (python's fold function)
def sum(l):
  def fold_fn(acc,elm): return acc + elm
  return reduce(fold_fn, l, 0)

def rev(l):
  def fold_fn(acc,elm): return [elm] + acc
  return reduce(fold_fn, l, [])

def print_some(l):
  def decorator(f):
    def decorated(*args):
      for i in l:
        if i in range(len(args)):
          print "Arg " + str(i) + ": " + str(args[i])
      res = f(*args)    
      if (-1) in l:
        print "Return: " + str(res)
      return res
    return decorated
  return decorator

@print_some([-1,1,0])
def plus(x,y):
  print "-- plus called --"
  return x+y

plus(1,2)

#class version
def print_some(l):
  class decorator:
    def __init__(self,f):
        self.f = f
        self.__name__ = f.__name__
    def __call__(self,*args):
      for i in l:
        if 0 <= i < len(args):
          print ‘Arg ‘ + str(i) + ‘: ‘ + str(args[i])
      x = self.f(*args)
      if -1 in l:
        print ‘Return: ‘ + str(self.f(*args))
      return x

#PROBLEM 7: Unification

Class Tree:
  def __init__(self, name, children):
    self.name = name
    self.children = children
  
  # Returns True if the Tree represents a prolog variable (e.g. X), and False otherwise
  def is_var(self): ...

  # Returns the string representation of the Tree as a Prolog term.
  def __repr__(self): ...

  # Constructs a Tree representing a Prolog variable with name n
  def var(n): return Tree(n, [])
  
  # Constructs a Tree representing a non-variable term with name n and children c
  def node(n, c): return Tree(n, c)

def apply_to_tree(s,t):
  if not t.is_var():
    return node(t.name, [apply_to_tree(s,x) for x in t.children])
  elif t.name in s:
    return apply_to_tree(s, s[t.name])
  else:
    return var(t.name)

def unify (a, b, s={}):
    a = apply_to_tree(s, a)
    b = apply_to_tree(s, b)
    result = s.copy()
    if a.is_var() and b.is_var():
        s[a.name] = b
    elif a.is_var() and not b.is_var():
        if a.name in result:
            result = unify(a,b,s)
        else:
            result[a.name] = b
    elif not a.is_var() and b.is_var():
        return unify(b,a,s)
    elif not a.is_var() and not b.is_var():
        if a.name != b.name:
            return False
        if len(a.children) != len(b.children):
            return False
        for i in range(0, len(a.children)):
            result = unify(a.children[i], b.children[i], result)
    return result


""" ------------------------------------------------- """
""" WINTER 2013 """

def transpose(m):
  height = len(m)
  width = len(m[0])
  return [ [ m[i][j] for i in  range(height)] for j in range(width) ]

def access(g, x, y):
  try: return g[y][x]
  except: return 0

g = [[ 0, 0, 0, 0, 0 ],
     [ 0, 0, 0, 0, 0 ],
     [ 0, 1, 1, 1, 0 ],
     [ 0, 0, 0, 0, 0 ],
     [ 0, 0, 0, 0, 0 ]]

def count_live_neighbors(g,x,y):
  live = 0
  for x_delta in [ -1, 0, 1]:
    for y_delta in [ -1, 0, 1]:
      if not (x_delta==0 and y_delta==0):
        live += access(g,x+x_delta,y+y_delta)
  return live

def new_val(g,x,y):
  neighbors = count_live_neighbors(g,x,y)
  if (access(g,x,y) != 0):
    if neighbors < 2 or neighbors > 3:
      return 0
    elif neighbors == 2 or neighbors == 3:
      return 1
  else:
    if neighbors == 3:
      return 1
    else:
      return 0

def step(g):
  height = len(g)
  width = len(g[0])
  return [ [ new_val(g,i,j) for i in range(width) ] for j in range(height) ]

def lift_1(f):
  def decorated(x):
    return [f(i) for i in x]
  return decorated

def lift_2(f):
  def decorated(x,y):
    return [ f(x[i],y[i]) for i in range(len(x)) ]
  return decorated

#alternate
def lift_2(f):
  def decorated(x,y):
    return [ f(i,j) for (i,j) in zip(x,y) ]
  return decorated

def lift(f):
  def decorated(*args):
    return [ f( *transpose(args)[i] ) for i in range(0, len(transpose(args))) ]
  return decorated

""" ------------------------------------------------- """
""" WINTER 2012 """

img1=[[ 11, 0, 12],
      [ 0, 0, 0],
      [ 13, 0, 14],
      [ 15, 16, 17]]

def square_img(img):
  return [ [ (i**2) for i in l] for l in img]

def crop_img(img,x1,y1,x2,y2):
  return [ img[i][x1:x2] for i in range(len(img)) if (i>=y1 and i < y2)]

#alternate
def crop_img(img,x1,y1,x2,y2):
  return[[img[l][w] for w in range(x1,x2)]for l in range(y1,y2)]

for l in crop_img(img1,0,1,2,4):
  string = ""
  for i in l:
    string = string + str(i) + '  '
  print string

def zip(l1,l2):
  end = min(len(l1),len(l2))
  ret = []
  for i in range(0,end):
    ret.append( (l1[i],l2[i]) )
  return ret

def add_imgs(img1, img2):
    return [[s1 + s2 for (s1, s2) in zip(l1, l2)] for (l1, l2) in zip(img1, img2)]

def derivative(d):
    def decorator(f):
        def decorated(arg):
            funk = (f(arg+d)-f(arg))/d
            print round(funk, 2)
        return decorated
    return decorator

""" ------------------------------------------------- """
""" WINTER 2011 """

def print_first_k_args(k):
  def decorator(f):
    def decorated(*args):
      for i in range(min(len(args),k)):
        #print "hello"
        print "Arg " + str(i+1) + ": " + str(args[i])
      res = f(*args)
      print "Return: " + str(res)
      return res
    return decorated
  return decorator

def create_image(w,h,c):
    return [ [c for i in range(0,w)] for j in range(0,h) ]

def well_formed(img):
    l = len(img)
    if l>0:
        lr = len(img[0])
        for i in range(0, l):
            if len(img[i]) != lr:
                return False
            else:
                for j in range(0, lr):
                    if (img[i][j]<0 or img[i][j]>255):
                        return False
    return True

def fill_rect(img, x0, y0, x1, y1, c):
    if x0<0:
        x0 = 0
    if y0<0:
        y0 = 0
    if well_formed(img):
        if y1 > len(img):
            y1 = len(img)
        if x1 > len(img[0]):
            x1 = len(img[0])
        for i in range(x0, x1):
            for j in range(y0, y1):
                img[j][i] = c
    return img

def fill_region(img, oldcolor, newcolor, x, y):
    img[y][x] = newcolor
    for(x1, y1) in [(x,y+1), (x,y-1), (x-1,y), (x+1,y)]:
        try:
            if not((x1<0) or (y1<0)) and img[y1][x1] == oldcolor:
                img = fill_region(img, oldcolor, newcolor, x1, y1)
        except:
            pass
    return img

