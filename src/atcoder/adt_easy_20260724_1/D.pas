program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NEED = 16 * 7;
var
	i, j, j1, j2, j3: int8;
	ans: int32;
	a: array [1 .. 3, 1 .. 6] of int8;
	have: uint8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to 3 do
		for j := 1 to 6 do read(a[i, j]);

	ans := 0;
	for j1 := 1 to 6 do
		for j2 := 1 to 6 do
			for j3 := 1 to 6 do begin
				have := (1 shl a[1, j1]) or (1 shl a[2, j2]) or (1 shl a[3, j3]);
				if have = NEED then inc(ans);
			end;

	writeln(ans / 216 :0:6);
end.
