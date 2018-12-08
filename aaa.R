num_sign=function(a,b,sign){
  if(sign==1){
    return(a+b)
  }
  else if(sign==2){
    return(a-b)
  }
  else if(sign==3){
    return(a*b)
  }
  else if(sign==4){
    return(a/b)
  }
  else if(sign==5){
    return(b-a)
  }
  else if(sign==6){
    return(b/a)
  }
}


num_reduc=function(A){
  len=length(A)
  B=vector(mode="list",length=0)
  for(i in 1:(len-1)){
    m=i+1
    for (j in m:len){
      for(k in 1:6){
        result=num_sign(A[i],A[j],k)
        temp_list=A[-i]
        temp_list=list(c(temp_list[-(j-1)],result))
        B=c(B,temp_list)
      }
    }
  }

  return(B)
}


num_reduc(c(1,2,3))

recur=function(A){
  B=op_reduc(A)
  times=length(A)-2
  for(j in 1:times){
  num=length(B)
  len=length(B[[1]])
  C=unlist(B)
  D=vector(mode="list",length=0)
  k=num-1
  for(i in 0:k){
    values=C[(i*len+1):((i+1)*len)]
    temp_list=op_reduc(values)
    D=c(D,temp_list)
    B=D
  } 
  }
  return(B)
}
recur(c(10,10,10))
recur(A)

yes_no=function(A,b){
  len=length(A)
  if(len==1){
    if(A==b){
      print(paste(b,"is just what you want"))
    }
    else{
      print(paste("Test your IQ please!"))
    }
  }
    else if(len==2){
      K=num_reduc(A)
      K=unlist(K)
      K=unique(K)

      count=0
      for(i in 1:length(K)){
        if(K[i]==b){
          count=count+1
        }
      }
        if(count>=1){
          print(paste("This is at least one way to get", b))
        }
        else{
          print(paste("We cant get",b,"with the number you provide"))
        }
      }
    
  else if(len>2){
    S=recur(A)
    S=unlist(S)
    S=unique(S)
  
    count1=0
    for(i in 1:length(S)){
      if(S[i]==b){
        count1=count1+1
      }
    }
      if(count1>=1){
        print(paste("This is at least one way to get", b))
      }
      else{
        print(paste("We cant get",b,"with the number you provide"))
      }
    }
    
  }
  
yes_no(c(1,2,3),10)
yes_no(c(1,2,3),100)
yes_no(c(1,2,3,4,5),10)
yes_no(10,10)
yes_no(1,10)

