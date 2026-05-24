program _C2;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
const
	NN = 200 * 1000;
var
	notc, tci, n, i, j, k, l, r: int32;
	p: int8;
	s: int64;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		k := 0;
		p := 1;

		s := 0;
		r := n;
		for l := n downto 1 do begin
			while (r > l) and (a[r] < 0) do begin
				dec(s, a[r]);
				dec(r);
			end;
			inc(s, a[l]);
			if (s < 0) then ;
		end;

		writeln(k);
		for j := 1 to k-1 do write(b[j], ' ');
		if k > 0 then write(b[k]);
		writeln;

	end;
end.
