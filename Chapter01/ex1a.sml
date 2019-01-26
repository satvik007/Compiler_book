type key = string;
datatype tree = LEAF | TREE of tree * key * tree;

val empty = LEAF;

fun insert (key, LEAF) = TREE (LEAF, key, LEAF)
    | insert (key, TREE (l, k, r)) = 
        if key < k
            then TREE (insert (key, l), k, r)
        else if key > k
            then TREE (l, k, insert (key, r))
        else TREE (l, key, r);

fun lookup (key, TREE (l, k, r)) = 
    if (key > k) then lookup (key, r)
    else (
        if (key < k) then lookup (key, l)
        else true
    )
    | lookup (key, LEAF) = false;

val a = TREE (TREE (LEAF, "3", LEAF), "4", TREE (TREE (LEAF, "5", LEAF), "6", LEAF));

lookup ("2", a);
lookup ("3", a);
lookup ("5", a);
lookup ("7", a);


