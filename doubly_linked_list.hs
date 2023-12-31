data DoublyLinkedList a = Nil | DLL {left :: DoublyLinkedList a, value :: a, right :: DoublyLinkedList a}

empty :: DoublyLinkedList a
empty = Nil

single :: a -> DoublyLinkedList a
single x = DLL {left = Nil, value = x, right = Nil}

pair :: a -> a -> DoublyLinkedList a
pair x y = left_node
  where
    left_node = DLL {left = Nil, value = x, right = right_node}
    right_node = DLL {left = left_node, value = y, right = Nil}

triple :: a -> a -> a -> DoublyLinkedList a
triple x y z = left_node
  where
    left_node = DLL {left = Nil, value = x, right = middle_node}
    middle_node = DLL {left = left_node, value = y, right = right_node}
    right_node = DLL {left = middle_node, value = z, right = Nil}

infinite_list :: a -> DoublyLinkedList a
infinite_list x = node
  where
    node = DLL {left = node, value = x, right = node}

go_right :: DoublyLinkedList a -> DoublyLinkedList a
go_right Nil = Nil
go_right node@(DLL l x Nil) = node
go_right (DLL l x r) = go_right r

go_left :: DoublyLinkedList a -> DoublyLinkedList a
go_left Nil = Nil
go_left node@(DLL Nil x r) = node
go_left (DLL l x r) = go_left l

append_right :: a -> DoublyLinkedList a -> DoublyLinkedList a
append_right x Nil = DLL {left = Nil, value = x, right = Nil}
append_right x node@(DLL l y Nil) = new_right_node
  where
    new_right_node = DLL {left = replace_right new_right_node node, value = x, right = Nil}
append_right x (DLL l y r) = append_right x r

append_left :: a -> DoublyLinkedList a -> DoublyLinkedList a
append_left x Nil = DLL {left = Nil, value = x, right = Nil}
append_left x node@(DLL Nil y r) = new_left_node
  where
    new_left_node = DLL {left = Nil, value = x, right = replace_left new_left_node node}
append_left x (DLL l y r) = append_left x l

replace_right :: DoublyLinkedList a -> DoublyLinkedList a -> DoublyLinkedList a
replace_right new_right_node (DLL l x r) = new_node
  where
    new_node = DLL {left = new_left_node, value = x, right = new_right_node}
    new_left_node = case l of
      Nil -> Nil
      left_node -> replace_right new_node left_node

replace_left :: DoublyLinkedList a -> DoublyLinkedList a -> DoublyLinkedList a
replace_left new_left_node (DLL l x r) = new_node
  where
    new_node = DLL {left = new_left_node, value = x, right = new_right_node}
    new_right_node = case r of
      Nil -> Nil
      right_node -> replace_left new_node right_node 

replace_value :: a -> DoublyLinkedList a -> DoublyLinkedList a
replace_value x (DLL l _ r) = new_node
  where
    new_node = DLL {left = new_left_node, value = x, right = new_right_node}
    new_left_node = case l of
      Nil -> Nil
      left_node -> replace_right new_node left_node
    new_right_node = case r of
      Nil -> Nil
      right_node -> replace_left new_node right_node

concatenate :: DoublyLinkedList a -> DoublyLinkedList a -> DoublyLinkedList a
concatenate Nil Nil = Nil
concatenate xs Nil = xs
concatenate Nil ys = ys
concatenate xs ys = new_left_node
  where
    new_left_node = DLL {left = replace_right new_left_node (left left_node), value = (value left_node), right = new_right_node}
    new_right_node = DLL {left = new_left_node, value = (value right_node), right = replace_left new_right_node (right right_node)}
    left_node = go_right xs
    right_node = go_left ys

instance Functor DoublyLinkedList
  where
    fmap :: (a -> b) -> (DoublyLinkedList a -> DoublyLinkedList b)
    fmap f Nil = Nil
    fmap f (DLL l x r) = new_node
      where
        new_node = DLL {left = new_left_node, value = f x, right = new_right_node}
        new_left_node = fmap f l
        new_right_node = fmap f r

from_list :: [a] -> DoublyLinkedList a
from_list xs = from_list_helper Nil xs
  where
    from_list_helper :: DoublyLinkedList a -> [a] -> DoublyLinkedList a
    from_list_helper left_node [] = Nil
    from_list_helper left_node (x:xs) = new_node
      where
        new_node = DLL {left = left_node, value = x, right = right_node}
        right_node = from_list_helper new_node xs

from_list' :: [a] -> DoublyLinkedList a
from_list' [] = Nil
from_list' (x:xs) = append_left x (from_list' xs)

fold' :: forall a b. (a -> b -> b) -> (a -> b -> b) -> (a -> b -> b) -> (b -> b) -> b -> b -> DoublyLinkedList a -> b
fold' left_f mid_f right_f left_default mid_default right_default t = fold_inner t
  where
    fold_inner :: DoublyLinkedList a -> b
    fold_inner Nil = mid_default
    fold_inner (DLL l x r) = left_fold l (mid_f x (right_fold r))
      where
        right_fold :: DoublyLinkedList a -> b
        right_fold Nil = right_default
        right_fold (DLL l x r) = right_f x (right_fold r)
        left_fold :: DoublyLinkedList a -> b -> b
        left_fold Nil acc = left_default acc
        left_fold (DLL l x r) acc = left_fold l (left_f x acc)

to_list :: DoublyLinkedList a -> [a]
to_list = fold' (:) (:) (:) id [] []

instance Show a => Show (DoublyLinkedList a)
  where
    show :: DoublyLinkedList a -> String
    show = fold' (\x acc -> "(" ++ (show x) ++ ")." ++ acc) (\x acc -> "(" ++ (show x) ++ ")" ++ acc) (\x acc -> ".(" ++ (show x) ++ ")" ++ acc) (\acc -> "[" ++ acc) "[]" "]"

elem' :: (Eq a) => a -> DoublyLinkedList a -> Bool
elem' q = fold' (\x acc -> (q == x) || acc) (\x acc -> (q == x) || acc) (\x acc -> (q == x) || acc) id False False

id' :: DoublyLinkedList a -> DoublyLinkedList a
id' = fold' (\x acc -> append_left x acc) (\x acc -> append_left x acc) (\x acc -> append_left x acc) id Nil Nil

repeat' :: Integer -> (a -> a) -> (a -> a)
repeat' 0 f x = x
repeat' n f x = repeat' (n - 1) f (f x)

example :: Integer -> DoublyLinkedList String
example n = append_right "Boundary-Right" $ append_left "Boundary-Left"
            $ replace_value "Middle" $ repeat' n right $ go_left
            $ replace_value "Next-To-Right" $ left $ go_right
            $ replace_value "Next-To-Left" $ right $ go_left
            $ fmap show
            $ fmap (\n -> n^2)
            $ from_list [1..(2*n + 1)]

example' :: DoublyLinkedList String
example' = concatenate (example 3) (example 4)
-- [("Boundary-Left").("1").("Next-To-Left").("9").("Middle").("25").("Next-To-Right").("49").("Boundary-Right").("Boundary-Left").("1").("Next-To-Left").("9").("16").("Middle").("36").("49").("Next-To-Right").("81").("Boundary-Right")]
