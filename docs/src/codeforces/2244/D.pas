program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, math;
const
	NN = 200 * 1000;
var
	notc, tci, n, i, m, j, b: int32;
	l, r: int64;
	a: array [1 .. NN] of int32;
	mot: TDictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m);

		for i := 1 to n do read(a[i]);
		readln;

		mot := TDictionary<int32, boolean>.Create;
		for j := 1 to m do begin
			read(b);
			mot.AddOrSetValue(b, true);
		end;
		readln;

		l := 0;
		r := 0;
		for i := 1 to n do begin

			inc(l, a[i]);
			inc(r, a[i]);

			if mot.ContainsKey(i) then begin
				r := max(abs(l), abs(r));
				l := - r;
			end;

		end;

		r := max(l, r);
		writeln(r);
		mot.Free;

	end;
end.
