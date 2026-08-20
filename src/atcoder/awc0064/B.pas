program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #tlist #default #sort
uses
	generics.collections,
	generics.defaults;
var
	n, i, ai: int32;
	score: int64;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	a := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(ai);
		a.add(ai);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	score := 0;
	for i := n-1 downto 0 do
		if odd(n-i) then
			inc(score, a[i])
		else
			dec(score, a[i]);

	writeln(score);
	a.free;
end.
