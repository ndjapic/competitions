program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #stack #deque
uses
	generics.collections;
const
	NN = 500 * 1000;
var
	n, i: int32;
	direction: boolean;
	s: string;
	l, r: tstack<int32>;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);
	direction := true;

	l := tstack<int32>.create;
	r := tstack<int32>.create;
	for i := 1 to n do begin
		if direction then
			r.push(i)
		else
			l.push(i);

		if s[i] = 'o' then direction := not direction;
	end;

	if direction then begin

		i := 1;
		while l.count > 0 do begin
			a[i] := l.pop;
			inc(i);
		end;

		i := n;
		while r.count > 0 do begin
			a[i] := r.pop;
			dec(i);
		end;

	end else begin

		i := 1;
		while r.count > 0 do begin
			a[i] := r.pop;
			inc(i);
		end;

		i := n;
		while l.count > 0 do begin
			a[i] := l.pop;
			dec(i);
		end;

	end;
	l.free;
	r.free;

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.
