program setrehv2;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords} 
uses
    gset;
type
    TLess = record
        class function c(L, R: int32): Boolean; static;
    end;
    TintSet = specialize TSet<int32, TLess>;
var
    n, i: int32;
    s: TintSet;
    it : TintSet.TIterator;
    a: array of int32;

class function TLess.c(L, R: int32): Boolean;       // Our sorting function
begin
    Result := L < R;
end;

begin
    s := TintSet.Create;
    s.Insert(1);
    s.Insert(4);
    s.Insert(16);
    s.Insert(64);
    s.Insert(256);
    s.Insert(1024);
    s.Insert(4096);
    s.Insert(16384);
    s.Insert(65536);
    s.Insert(1); // Note, a duplicate !
    s.Insert(5);
    s.Insert(25);
    s.Insert(125);
    s.Insert(625);
    s.Insert(3125);
    s.Insert(15625);
    s.Insert(78125);
    
    it := s.Min;
    n := 0;
    setlength(a, 1);
    if it <> nil then begin
        repeat
            if length(a) = n then setlength(a, 2*n);
            a[n] := it.GetData;
            inc(n);
        until not it.Next;
    end;
    it.Free;
    s.Free;

    for i := 0 to n-2 do write(a[i], ' '); writeln(a[n-1]);
end.
