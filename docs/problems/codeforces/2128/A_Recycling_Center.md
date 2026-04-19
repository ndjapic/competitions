# Problem: A_Recycling_Center.pas

```pascal
program A_Recycling_Center;
{$MODE DELPHI}{$INLINE ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes;
type
	TIntComparer = class(TComparer<int32>)
		function Compare(constref L, R: int32): Integer; override;
	end;
var
	ntc, tci, n, c, i, ans: int32;
	a: TList<int32>;
	sl: TStringList;
	ios: string;
	Comparer: TIntComparer;

function TIntComparer.Compare(constref L, R: int32): Integer;
begin
	Result := L - R;
end;

begin
	randomize;
	sl := TStringList.Create;
	sl.Delimiter := ' ';
	a := TList<int32>.Create;
	Comparer := TIntComparer.Create;
	Comparer._AddRef;

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, c);

		readln(ios);
		sl.DelimitedText := ios;

		a.Clear;
		for i := 0 to n-1 do begin
			a.Add(StrToInt(sl[i]));
			a.Exchange(i, random(i+1));
		end;
		a.Sort(Comparer);

		ans := n;
		for i := n-1 downto 0 do
			if a[i] <= c then begin
				dec(ans);
				c := c div 2;
			end;

		sl.Clear;
		sl.Add(IntToStr(ans));
		writeln(sl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(sl);
	FreeAndNil(a);
	Comparer._Release;
end.

```
