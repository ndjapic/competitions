program gsetutil;
uses
    gvector, gset, gutil;
type
    iLess = specialize TLess<int32>;
    iVector = specialize TVector<int32>;
    iSet = specialize TSet<int32, iLess>;
var
    V: iVector;
    S: iSet;
    N, i: int32;
    it: iSet.TIterator;

begin
    readln(N);
    randomize;

    V := iVector.Create();
    for i:=1 to N do V.PushBack(random(N));

    for i := 0 to v.size()-1 do write(' ', v[i]); writeln;

    S := iSet.Create();
    for i:=0 to N-1 do S.Insert(V[i]);
    V.Clear();

    it := S.Min();
    repeat
        V.PushBack( it.GetData() );
    until not it.Next();

    for i := 0 to v.size()-1 do write(' ', v[i]); writeln;
end.
