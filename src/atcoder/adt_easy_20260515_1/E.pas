program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bitmask
uses
	generics.collections;
const
	NN = 15;
var
	n, m, k, i, j, c, a: int8;
	r: char;
	have: boolean;
	mask, s: int32;
	need: array [0 .. 1 shl NN] of boolean;
	nobs: array [0 .. 1 shl NN] of int8;
	satisfies: tdictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	nobs[0] := 0;
	satisfies := tdictionary<int32, boolean>.create;

	for mask := 0 to (1 shl n) - 1 do begin
		satisfies.addorsetvalue(mask, true);
		nobs[mask] := nobs[mask div 2] + mask mod 2;
		need[mask] := nobs[mask] >= k;
	end;

	for i := 1 to m do begin
		read(c);

		s := 0;
		for j := 1 to c do begin
			read(a);
			inc(s, 1 shl a);
		end;
		s := s div 2;

		readln(r, r);
		have := r = 'o';

		for mask in satisfies.keys.toarray do
			if need[mask and s] <> have then
				satisfies.remove(mask);
	end;

	writeln(satisfies.count);
	satisfies.free;
end.
