program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #mex #learn
const
	RR = 20;
var
	r, c, i, j, o, x, y: int8;
	b, a: array [1 .. RR] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure empty(i, j: int8);
begin
	if (1 <= i) and (i <= r) and (1 <= j) and (j <= c) then a[i][j] := '.';
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, c);

	for i := 1 to r do begin
		readln(b[i]);
		a[i] := b[i];
	end;

	for i := 1 to r do
		for j := 1 to c do begin
			o := ord(b[i][j]) - ord('0');
			if (1 <= o) and (o <= 9) then begin
				a[i][j] := '.';
				for x := 1 to o do
					for y := 0 to o-x do begin
						empty(i+x, j+y);
						empty(i-y, j+x);
						empty(i-x, j-y);
						empty(i+y, j-x);
					end;
			end;
		end;

	for i := 1 to r do writeln(a[i]);
end.
