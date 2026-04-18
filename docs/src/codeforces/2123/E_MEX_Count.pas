program E_MEX_Count;
uses
	math;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, k, x, mex: int32;
	c, b, ans: array [0 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for x := 0 to n do c[x] := 0;

		for i := 1 to n do begin
			read(x);
			inc(c[x]);
		end;
		readln;

		mex := 0;
		while c[mex] > 0 do inc(mex);

		for k := 0 to n do b[k] := 0;

		for x := 0 to mex do inc(b[c[x]]);

		ans[0] := 1;
		for k := 1 to n do
			ans[k] := ans[k-1] + b[k];

		for k := 1 to n do ans[k] := min(ans[k], n+1-k);

		for k := 0 to n-1 do write(ans[k], ' ');
		writeln(ans[n]);

	end;
end.
