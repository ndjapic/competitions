program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000;
var
	n, i, j: int32;
	a: array [1 .. NN] of string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(a[i]);

	ans := true;
	for i := 1 to n do
		for j := 1 to n do
			if ans then
				case a[i][j] of
					'-': ;
					'W': ans := a[j][i] = 'L';
					'L': ans := a[j][i] = 'W';
					'D': ans := a[j][i] = 'D';
				end;

	if ans then
		writeln('correct')
	else
		writeln('incorrect');
end.
