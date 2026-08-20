program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils, strutils, generics.collections;
const
	NN = 100;
var
	n, m, i, j: int8;
	p, p0, ans: int32;
	line: string;
	c, d: TStringArray;
	price: tdictionary<string, int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	readln(line);
	c := SplitString(line, ' '); 

	readln(line);
	d := SplitString(line, ' '); 

	price := tdictionary<string, int32>.create;
	read(p0);
	for j := 0 to m-1 do begin
		read(p);
		price.AddOrSetValue(d[j], p);
	end;
	readln;

	ans := 0;
	for i := 0 to n-1 do begin
		if not price.TryGetValue(c[i], p) then p := p0;
		inc(ans, p);
	end;

	writeln(ans);
	price.free;
end.
