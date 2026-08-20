program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 10;
var
	n: int8;
	ans: int32;
	p: array [1 .. NN] of int8;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function pernum(): int32;
var
	i, j: int8;
	f: int32;
begin
	for i := 1 to n do begin
		read(p[i]);
		seen[p[i]] := true;
	end;
	readln;

	f := 1;
	result := 0;
	for i := n downto 1 do begin
		for j := 1 to p[i]-1 do
			if not seen[j] then inc(result, f);
		seen[p[i]] := false;
		f := f * (n-i+1);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := - pernum() + pernum() - 1;

	writeln(max(0, ans));
end.
