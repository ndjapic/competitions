program B_Robot_Weight;
const
	nn = 100;
var
	x, n, i, q, p: int32;
	w: array [1 .. nn] of int32;

begin
	readln(x);
	readln(n);

	for p := 1 to n do read(w[p]); readln;

	readln(q);

	for i := 1 to q do begin
		readln(p);
		inc(x, w[p]);
		w[p] := -w[p];
		writeln(x);
	end;
end.
