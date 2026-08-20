program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, math;
const
	NN = 200 * 1000;
var
	notc, tci, n, i, o, c: int32;
	x: int64;
	valid: boolean;
	a, b: array [1 .. NN] of int64;
	d: TDictionary<int64, int32>;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int32): int32;
begin
	Result := CompareValue(b[l], b[r]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		d := TDictionary<int64, int32>.Create;
		p := TList<int32>.Create;
		try

			for i := 1 to n do begin
				read(x);
				b[i] := x;
				if not d.TryGetValue(x, c) then c := 0;
				d.AddOrSetValue(x, c+1);
				p.Add(i);
				p.Exchange(i-1, Random(i));
			end;
			readln;
			p.Sort(TComparer<int32>.Construct(cmp));

			valid := b[p[0]] = 0;

			for o := 0 to n-1 do begin
				i := p[o];
				if valid and d.TryGetValue(b[i], c) then begin
					if c = n then
						a[i] := 1
					else if (o > 0) and (b[i] = b[p[o-1]]) then
						a[i] := a[p[o-1]]
					else if o+c = n then
						a[i] := a[p[o-1]] + 1
					else if (b[p[o+c]] - b[i]) mod c > 0 then
						valid := false
					else begin
						a[i] := (b[p[o+c]] - b[i]) div c;
						valid := (o = 0) or (a[i] > a[p[o-1]]);
					end;
				end;
			end;

			if valid then begin
				for i := 1 to n-1 do write(a[i], ' ');
				writeln(a[n]);
			end else
				writeln(-1);

		finally
			d.Free;
			p.Free;
		end;

	end;
end.
