# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, v, i, d, k: int32;
	t: int64;
	s: array [1 .. nn] of int64;
	ans: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, v);

	s[1] := 0;
	for i := 1 to n-1 do begin
		read(d);
		s[i+1] := s[i] + d;
	end;
	readln;

	k := 0;
	for i := 2 to n do begin
		read(t);
		if s[i] div v < t then begin
			inc(k);
			ans[k] := i;
		end;
	end;
	readln;

	if k = 0 then
		writeln(-1)
	else begin
		for i := 1 to k-1 do write(ans[i], ' ');
		writeln(ans[k]);
	end;
end.

```
