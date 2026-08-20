program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
var
	n: int64;
	i: int8;
	d: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	dec(n);

	d := tlist<int8>.create;
	if n = 0 then d.add(0);

	while n > 0 do begin
		d.add(n mod 5);
		n := n div 5;
	end;

	for i := d.count - 1 downto 0 do write(d[i] * 2);
	writeln;
	d.free;
end.
