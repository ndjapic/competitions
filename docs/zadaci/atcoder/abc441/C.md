# Задатак: C.pas

```pascal
program C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
type
	TPair = class
		a: int64;
		c: int32;
	end;
	TPairComparer = class(TComparer<TPair>)
		function Compare(constref Left, Right: TPair): Integer; override;
	end;
var
	n, m, i, j, k, ai: int32;
	x, s: int64;
	a: TList<int32>;
	b: TObjectList<TPair>;
	Comparer: TPairComparer;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function TPairComparer.Compare(constref Left, Right: TPair): Integer;
begin
	if Left.a < Right.a then
		Result := -1
	else if Left.a > Right.a then
		Result := 1
	else if Left.c > Right.c then
		Result := -1
	else if Left.c < Right.c then
		Result := 1
	else
		Result := 0;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;
	a := TList<int32>.Create;
	b := TObjectList<TPair>.Create(True);
	Comparer := TPairComparer.Create;
	Comparer._AddRef;
	try

		readln(n, k, x);

		for i := 0 to n-1 do begin
			read(ai);
			a.Add(ai);
			a.Exchange(i, random(i+1));
		end;
		readln;
		a.Sort;

		m := 0;
		for i := 0 to n-1 do begin
			if (i = 0) or (a[i-1] < a[i]) then begin
				b.Add(TPair.Create);
				b[m].a := 0;
				b[m].c := 0;
				inc(m);
			end;
			inc(b[m-1].a, a[i]);
			inc(b[m-1].c);
		end;
		{writeln('m=',m);}

		for j := 0 to m-1 do b.Exchange(j, random(j+1));
		b.Sort(Comparer);
		{for j := 0 to m-1 do writeln('j=',j, ' b[j]=(', b[j].a, ', ', b[j].c, ')');}

		i := 0;
		j := 0;
		while (j < m) and (i < k) do begin
			inc(i, b[j].c);
			inc(j);
		end;

		s := 0;
		{writeln('i=',i, ' j=',j, ' s=',s, ' x=',x);}
		while (j > 0) and (s < x) do begin
			dec(j);
			inc(s, b[j].a);
			dec(i, b[j].c);
			{writeln('i=',i, ' j=',j, ' s=',s, ' x=',x);}
		end;

		if s < x then
			writeln(-1)
		else
			writeln(n-i);

	finally
		a.Free;
		b.Free;
		Comparer._Release;
	end;
end.

```
