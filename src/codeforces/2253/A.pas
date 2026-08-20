program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 3000 * 1000 + 1;
var
	notc, tci, n, x, y: int32;
	isPrime: array [2 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for x := 2 to NN do isPrime[x] := true;

	x := 2;
	while x*x <= NN do begin
		if isPrime[x] then begin
			y := 2*x;
			while y <= NN do begin
				isPrime[y] := false;
				inc(y, x);
			end;
		end;
		inc(x);
	end;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		if isPrime[n+1] then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
