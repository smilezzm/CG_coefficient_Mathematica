below is the plain code in the file. 

(*mjlist looks like \
{{C1,{m1,j1},{m2,j2}},{C2,{m1+1,j1},{m2-1,j2}},...}*)
JUp1[mjlist_List] := Module[{result, i, A},
   result = {};
   For[i = 1, i <= Length[mjlist], i++,
    If[mjlist[[i]][[2]][[1]] >= mjlist[[i]][[2]][[2]], Null, 
     AppendTo[
      result, {mjlist[[i]][[1]]*
        Sqrt[mjlist[[i]][[2]][[2]]*(mjlist[[i]][[2]][[2]] + 1) - 
          mjlist[[i]][[2]][[
            1]]*(mjlist[[i]][[2]][[1]] + 1)], {mjlist[[i]][[2]][[
          1]] + 1, mjlist[[i]][[2]][[2]]}, mjlist[[i]][[3]]}]]];
   result];
JDown1[mjlist_List] := Module[{result, i, A},
   result = {};
   For[i = 1, i <= Length[mjlist], i++,
    If[mjlist[[i]][[2]][[1]] <= -mjlist[[i]][[2]][[2]], Null, 
     AppendTo[
      result, {mjlist[[i]][[1]]*
        Sqrt[mjlist[[i]][[2]][[2]]*(mjlist[[i]][[2]][[2]] + 1) - 
          mjlist[[i]][[2]][[
            1]]*(mjlist[[i]][[2]][[1]] - 1)], {mjlist[[i]][[2]][[
          1]] - 1, mjlist[[i]][[2]][[2]]}, mjlist[[i]][[3]]}]]];
   result];
JUp2[mjlist_List] := Module[{result, i, A},
   result = {};
   For[i = 1, i <= Length[mjlist], i++,
    If[mjlist[[i]][[3]][[1]] >= mjlist[[i]][[3]][[2]], Null, 
     AppendTo[
      result, {mjlist[[i]][[1]]*
        Sqrt[mjlist[[i]][[3]][[2]]*(mjlist[[i]][[3]][[2]] + 1) - 
          mjlist[[i]][[3]][[1]]*(mjlist[[i]][[3]][[1]] + 1)], 
       mjlist[[i]][[2]], {mjlist[[i]][[3]][[1]] + 1, 
        mjlist[[i]][[3]][[2]]}}]]];
   result];
JDown2[mjlist_List] := Module[{result, i, A},
   result = {};
   For[i = 1, i <= Length[mjlist], i++,
    If[mjlist[[i]][[3]][[1]] <= -mjlist[[i]][[3]][[2]], Null, 
     AppendTo[
      result, {mjlist[[i]][[1]]*
        Sqrt[mjlist[[i]][[3]][[2]]*(mjlist[[i]][[3]][[2]] + 1) - 
          mjlist[[i]][[3]][[1]]*(mjlist[[i]][[3]][[1]] - 1)], 
       mjlist[[i]][[2]], {mjlist[[i]][[3]][[1]] - 1, 
        mjlist[[i]][[3]][[2]]}}]]];
   result];
(*Define a function to find the orthogonal vector*)
FindOrthogonalVector[vectors_List] := 
  Module[{A, nullVec, orthogonalVector}, 
   A = vectors;(*Use the input list of n-1 vectors*)
   nullVec = NullSpace[A];(*Find the null space*)
   If[Length[nullVec] > 0, 
    orthogonalVector = Normalize[First[nullVec]], 
    orthogonalVector = 
     "Error when finding the " <> ToString[Length[A] - 1] <> 
      "th orthogonal vectors"];
   orthogonalVector];
