program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	NN = 300 * 1000;
var
	n, m, i, j, x, a, ans: int32;
	k: array [1 .. NN] of int32;
	adj: array [1 .. NN] of tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do adj[a] := tlist<int32>.create;

	for j := 1 to m do begin
		read(k[j]);
		for x := 1 to k[j] do begin
			read(a);
			adj[a].add(j);
		end;
		readln;
	end;

	ans := 0;
	for i := 1 to n do begin
		read(a);

		for j in adj[a] do begin
			dec(k[j]);
			if k[j] = 0 then inc(ans);
		end;

		writeln(ans);
	end;
	readln;
end.
