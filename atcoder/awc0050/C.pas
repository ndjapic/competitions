program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults, sysutils;
const
	nn = 200 * 1000;
var
	n, i, j, k, t, x: int32;
	s: string;
	d: TDictionary<int32, int32>;
	c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	d := TDictionary<int32, int32>.create;
	t := 0;
	k := 0;
	x := 0;
	for i := 1 to n do begin
		readln(s);
		case s[1] of

			'P': begin
				inc(t);
				c[t] := StrToInt(RightStr(s, length(s) - 4));
				x := x xor c[t];
			end;

			'R': begin
				x := x xor c[t];
				dec(t);
			end;

			'L': begin
				inc(k);
				if d.TryGetValue(x, j) then
					WriteLn(j)
				else
					WriteLn(-1);
				d.AddOrSetValue(x, k);
			end;

		end;
	end;

	d.free;
end.
