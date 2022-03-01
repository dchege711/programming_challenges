This is a sample note.

\begin{code}

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

main :: IO ()
main = print (fact 5)

\end{code}

<a href='{{< ref "/computer-science/programming-challenges/advent-of-code/2021/src/HydrothermalVenture/05-hydrothermal-venture" }}'>
Trying Hugo Code</a>
