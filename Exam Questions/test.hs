triple :: a->b->c->(a->b->c->d)->d
triple x y z w = w x y z

fist :: a -> b -> c -> a
fist x y z = x
