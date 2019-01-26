type key = string;
type value = string;
datatype 'a tree = LEAF | TREE of 'a tree * key * value * 'a tree;

val empty = LEAF;

fun insert (key, value, LEAF) = TREE (LEAF, key, value, LEAF)
    | insert (key, value, TREE (l, k, v, r)) = 
        if key < k
            then TREE (insert (key, value, l), k, v, r)
        else if key > k
            then TREE (l, k, v, insert (key, value, r))
        else TREE (l, key, v, r);

fun lookup (key, TREE (l, k, v, r)) = 
        if (key > k) 
            then lookup (key, r)
        else if (key < k) 
            then lookup (key, l)
        else v
    | lookup (key, LEAF) = "-1";

val a = TREE (TREE (LEAF, "3", "a", LEAF), "4", "B", TREE (TREE (LEAF, "5", "t", LEAF), "6", "r", LEAF));

lookup ("2", a);
lookup ("3", a);
lookup ("5", a);
lookup ("7", a);


