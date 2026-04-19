# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Classes;
var
	n, k, i: int8;
	s: TStringList;
	p: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	s := TStringList.Create;
	try

		for i := 0 to n-1 do begin
			readln(p);
			if i < k then s.Add(p);
			s.Sort;
		end;

		for i := 0 to k-1 do begin
			writeln(s[i]);
		end;

	finally
		s.Free;
	end;
end.

```
