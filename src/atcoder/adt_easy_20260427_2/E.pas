program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
const
	nn = 50 * 1000;
var
	n, i: int32;
	j, k: int8;
	ai: int64;
	x, s: string;
	order: array ['a' .. 'z'] of int8;
	a: tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(x);
	for k := 1 to 26 do order[x[k]] := k;

	readln(n);

	a := tlist<int64>.create;
	for i := 0 to n-1 do begin
		readln(s);
		ai := 0;
		for j := 1 to length(s) do begin
			k := order[s[j]];
			inc(ai, int64(k) shl (50 - 5*j));
		end;
		a.add(ai);
		a.exchange(i, random(i+1));
	end;
	a.sort;

	for i := 0 to n-1 do begin
		for j := 1 to 10 do begin
			k := (a[i] shr (50 - 5*j)) and 31;
			if k > 0 then write(x[k]);
		end;
		writeln;
	end;
	a.free;
end.
