program B_Locked_Rooms;
const
	nn = 100;
var
	n, i, j: int8;
	l: array [1 .. nn] of int8;

begin
	readln(n);

	for i := 1 to n do read(l[i]); readln;

	i := 0;
	j := n;
	while (j-i > 1) and (l[i+1] = 0) do inc(i);
	while (j-i > 1) and (l[j] = 0) do dec(j);

	writeln(j-i-1);
end.
