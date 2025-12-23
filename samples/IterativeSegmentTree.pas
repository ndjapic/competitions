program IterativeSegmentTree;

const
  MAXN = 100000; // Maximum size of the array

var
  N: integer; // Actual size of the array
  tree: array[0..2 * MAXN - 1] of longint;

// Builds the segment tree from an initial array (optional)
procedure Build(arr: array of longint);
var
  i: integer;
begin
  for i := 0 to N - 1 do
    tree[N + i] := arr[i]; // Initialize leaves
  for i := N - 1 downto 1 do
    tree[i] := tree[2 * i] + tree[2 * i + 1]; // Build parents
end;

// Point increment update
procedure Update(idx: integer; val: longint);
begin
  idx := idx + N; // Adjust to tree index
  tree[idx] := tree[idx] + val; // Increment leaf
  while idx > 1 do
  begin
    idx := idx div 2; // Move to parent
    tree[idx] := tree[2 * idx] + tree[2 * idx + 1]; // Update parent
  end;
end;

// Range sum query
function Query(L, R: integer): longint;
var
  sum: longint;
begin
  sum := 0;
  L := L + N; // Adjust to tree index
  R := R + N; // Adjust to tree index
  while L <= R do
  begin
    if (L mod 2 = 1) then // If L is a right child
    begin
      sum := sum + tree[L];
      inc(L);
    end;
    if (R mod 2 = 0) then // If R is a left child
    begin
      sum := sum + tree[R];
      dec(R);
    end;
    L := L div 2; // Move to parent
    R := R div 2; // Move to parent
  end;
  Query := sum;
end;

begin
  // Example Usage:
  N := 5;
  // Initialize with some values (e.g., from an array)
  // For demonstration, let's manually set initial values in the tree leaves
  tree[N+0] := 1;
  tree[N+1] := 2;
  tree[N+2] := 3;
  tree[N+3] := 4;
  tree[N+4] := 5;
  Build([]); // Call Build if you have an initial array

  // Build the parent nodes for the initial values
  for N - 1 downto 1 do
    tree[i] := tree[2 * i] + tree[2 * i + 1];

  writeln('Initial sum of range [0, 4]: ', Query(0, 4)); // Expected: 15

  Update(2, 10); // Increment element at index 2 by 10
  writeln('Sum of range [0, 4] after update: ', Query(0, 4)); // Expected: 25

  writeln('Sum of range [1, 3]: ', Query(1, 3)); // Expected: 2+13+4 = 19
end.
