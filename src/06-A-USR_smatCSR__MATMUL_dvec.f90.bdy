nnz = A % IA (A % N + 1) - 1
t = A % A (1:nnz) * x ( A % JA (1:nnz) )
y = (/ (SUM( t (A % IA (i):A % IA (i+1)-1) ), i = 1, A % N) /)
