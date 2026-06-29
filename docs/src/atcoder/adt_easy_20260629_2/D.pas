program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #char #string
uses
	Generics.Collections,
	Generics.Defaults;
var
	n, i: int32;
	s: string;
	a: tlist<char>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(s);
	n := length(s);

	a := tlist<char>.create;
	for i := 1 to n do begin
		a.add(s[i]);
		a.exchange(i-1, random(i));
	end;
	a.sort;

	for i := 1 to n do s[i] := a[i-1];
	writeln(s);
	a.Free;
end.
