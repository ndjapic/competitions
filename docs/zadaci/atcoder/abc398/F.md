# Задатак: F.pas

```pascal
program _F;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils, StrUtils;
const
	nn = 500 * 1000;
var
	n, m, i, j: int32;
	s, rev, comb: string;
	kmp: array [1 .. 2*nn+1] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	rev := ReverseString(s);
	comb := rev + '#' + s;
	n := length(s);
	m := length(comb);

	kmp[1] := 0;
	j := 0;

	for i := 2 to m do begin
		while (j > 0) and (comb[i] <> comb[j+1]) do j := kmp[j];
		if comb[i] = comb[j+1] then inc(j);
		kmp[i] := j;
	end;

	write(s);
	if kmp[m] < n then write(RightStr(rev, n - kmp[m]));
	writeln;
end.

```
