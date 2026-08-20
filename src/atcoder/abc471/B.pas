program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashset
uses
	generics.collections, math;
var
	n, i, c, ans: int8;
	s: string;
	d: tdictionary<string, int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	d := tdictionary<string, int8>.create;
	ans := 0;

	for i := 1 to n do begin
		readln(s);
		s := LowerCase(s);
		if not d.trygetvalue(s, c) then c := 0;
		inc(c);
		d.addorsetvalue(s, c);
		ans := max(ans, c);
	end;

	writeln(ans);
	d.free;
end.
