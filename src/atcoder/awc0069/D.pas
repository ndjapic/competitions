program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #WA #default #sort
uses
	generics.collections,
	generics.defaults;
const
	NN = 200 * 1000;
var
	n, k, d, i, xi, p, l, r, ans: int32;
	x: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k, d);

	x := TList<int32>.Create;
	for i := 1 to n do begin
		ReadLn(xi, p);
		if p >= k then begin
			x.Add(xi);
			x.Exchange(x.Count - 1, Random(x.Count));
		end;
	end;

	n := x.Count;
	if n = 0 then
		ans := -1
	else begin

		x.Sort;
		ans := 0;
		l := 0;
		for i := 0 to n-1 do
			if (l <= i) and (l < n) and ((i+1 >= n) or (x[i+1] - x[l] > d)) then begin
				inc(ans);
				r := i;
				while (r+1 < n) and (x[r+1] - x[i] <= d) do inc(r);
				l := r+1;
			end;

	end;

	writeln(ans);
	x.Free;
end.
