program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
const
	PRIME = 998244353;
	NN = 1000 * 1000;
	EE = 30;
var
	notc, tci, n, i, k: int32;
	e: int8;
	ans: int64;
	a: array [1 .. NN] of int32;
	c: array [0 .. EE] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for e := 0 to EE do c[e] := 0;

		k := 0;
		for i := 1 to n do begin
			read(a[i]);
			if a[i] > 0 then inc(k);
			for e := 0 to EE do
				if odd(a[i] shr e) then inc(c[e]);
		end;
		readln;

		ans := 1;
		for e := 0 to EE do
			if odd(c[e]) then
				ans := ans * c[e] mod PRIME;
		if k <= 1 then ans := 0;

		writeln(ans);

	end;
end.
