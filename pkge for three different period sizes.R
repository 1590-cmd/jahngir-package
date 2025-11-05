#################################################################################
# classes of Minimal Minimal Circular Balanced Repeated Measurements
# Design for period of three different sizes (p1,p2 and p3)

# Algorithm from paper:

#  
# Coded by Jahngir et al., 2024-2025 
# Version 2.1.0  (2024-04-20)

#################################################################################

Designcheck<-function(v,p1,p2,p3,i){
  #v=9
  #p=4
  #if(i<=0)stop("i must be a positive integer")
  if(v%%2!=0){
    if((v-1-p2-p3)%%p1==0)
    {
    i=(v-1-p2-p3)/p1
    if(i>=1)
    cat("MCBRMD IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=1","\n")
    }
    if((v-1-p2-2*p3)%%p1==0)
    {
    i=(v-1-p2-2*p3)/p1
    if(i>=1)
    cat("MCBRMD IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=1","\n")
    }
    if((v-1-2*p2-p3)%%p1==0)
    {
    i=(v-1-2*p2-p3)/p1
    if(i>=1)
    cat("MCBRMD IS POSSIBLE with two sets of p2 and one sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=1","\n")
    }
    if((v-1-2*p2-2*p3)%%p1==0)
    {
    i=(v-1-2*p2-2*p3)/p1
    if(i>=1)
    cat("MCBRMD IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=1","\n")
    }
     
             
    if((v-p2-p3)%%p1==0)
    {
    i=(v-p2-p3)/p1
    if(i>=1)
    cat("MCSBRMD IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=2","\n")
    }
    if((v-p2-2*p3)%%p1==0)
    {
    i=(v-p2-2*p3)/p1
    if(i>=1)
    cat("MCSBRMD IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=2","\n")
    }
    if((v-2*p2-p3)%%p1==0)
    {
    i=(v-2*p2-p3)/p1
    if(i>=1)
    cat("MCSBRMD IS POSSIBLE with two sets of p2 and one set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=2","\n")
    }
    if((v-2*p2-2*p3)%%p1==0)
    {
    i=(v-2*p2-2*p3)/p1
    if(i>=1)
    cat("MCSBRMD IS POSSIBLE with two sets of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=2","\n")
    }
      
      
      if((v-3-p2-p3)%%p1==0)
      {
        i=(v-3-p2-p3)/p1
        if(i>=1)
        cat("MCPBRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=3","\n")
      }
        if((v-3-p2-2*p3)%%p1==0)
        {
          i=(v-3-p2-2*p3)/p1
          if(i>=1)
          cat("MCPBRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=3","\n")
        }
          if((v-3-2*p2-p3)%%p1==0)
          {
            i=(v-3-2*p2-2*p3)/p1
            if(i>=1)
            cat("MCPBRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=3","\n")
          }
            if((v-3-2*p2-2*p3)%%p1==0)
            {
             i=(v-3-2*p2-2*p3)/p1
              if(i>=1)
              cat("MCPBRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=3","\n")
            }  
      
  if((v-2-p2-p3)%%p1==0)
  {
  i=(v-2-p2-p3)/p1
  if(i>=1)
  cat("MCSPBRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=4","\n")
  }
  if((v-2-p2-2*p3)%%p1==0)
  {
  i=(v-2-p2-2*p3)/p1
  if(i>=1)
  cat("MCSPBRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=4","\n")
  }
  if((v-2-2*p2-p3)%%p1==0)
  {
  i=(v-2-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCSPBRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=4","\n")
  }
  if((v-2-2*p2-2*p3)%%p1==0)
  {
  i=(v-2-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCSPBRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=4","\n")
  }                    
      
      
    if((v-4-p2-p3)%%p1==0)
    {
      i=(v-4-p2-p3)/p1
      if(i>=1)
      cat("MCPBRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=5","\n")
    }
      if((v-4-p2-2*p3)%%p1==0)
      {
        i=(v-4-p2-2*p3)/p1
        if(i>=1)
        cat("MCPBRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=5","\n")
      }
        if((v-4-2*p2-p3)%%p1==0)
        {
          i=(v-4-2*p2-2*p3)/p1
          if(i>=1)
          cat("MCPBRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=5","\n")
        }
          if((v-4-2*p2-2*p3)%%p1==0)
          {
            i=(v-4-2*p2-2*p3)/p1
            if(i>=1)
            cat("MCPBRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=5","\n")
          }   
  
                     
   if((v-3-p2-p3)%%p1==0)
   {
   i=(v-3-p2-p3)/p1
   if(i>=1)
   cat("MCSPBRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=6","\n")
   }
   if((v-3-p2-2*p3)%%p1==0)
   {
   i=(v-3-p2-2*p3)/p1
   if(i>=1)
   cat("MCSPBRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=6","\n")
   }
   if((v-3-2*p2-p3)%%p1==0)
   {
   i=(v-3-2*p2-2*p3)/p1
   if(i>=1)
   cat("MCSPBRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=6","\n")
   }
   if((v-3-2*p2-2*p3)%%p1==0)
   {
   i=(v-3-2*p2-2*p3)/p1
   if(i>=1)
   cat("MCSPBRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=6","\n")
   }            
     
            
     if((v+1-p2-p3)%%p1==0)
     {
       i=(v+1-p2-p3)/p1
       if(i>=1)
       cat("MCWBRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=7","\n")
     }
       if((v+1-p2-2*p3)%%p1==0)
       {
         i=(v+1-p2-2*p3)/p1
         if(i>=1)
         cat("MCWBRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=7","\n")
       }
         if((v+1-2*p2-p3)%%p1==0)
         {
           i=(v+1-2*p2-2*p3)/p1
           if(i>=1)
           cat("MCWBRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=7","\n")
         }
           if((v+1-2*p2-2*p3)%%p1==0)
           {
             i=(v+1-2*p2-2*p3)/p1
             if(i>=1)
             cat("MCWBRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=7","\n")
           }   
             
            
    if((v+2-p2-p3)%%p1==0)
    {
    i=(v+2-p2-p3)/p1
    if(i>=1)
    cat("MCSBGRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=8","\n")
    }
    if((v+2-p2-2*p3)%%p1==0)
    {
    i=(v+2-p2-2*p3)/p1
    if(i>=1)
    cat("MCSBGRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=8","\n")
    }
    if((v+2-2*p2-p3)%%p1==0)
    {
    i=(v+2-2*p2-2*p3)/p1
    if(i>=1)
    cat("MCSBGRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=8","\n")
    }
    if((v+2-2*p2-2*p3)%%p1==0)
    {
    i=(v+2-2*p2-2*p3)/p1
    if(i>=1)
    cat("MCSBGRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=8","\n")
    }             

      
      if((v+2-p2-p3)%%p1==0)
      {
        i=(v+2-p2-p3)/p1
        if(i>=1)
        cat("MCWBRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=9","\n")
      }
        if((v+2-p2-2*p3)%%p1==0)
        {
          i=(v+2-p2-2*p3)/p1
          if(i>=1)
          cat("MCWBRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=9","\n")
        }
          if((v+2-2*p2-p3)%%p1==0)
          {
            i=(v+2-2*p2-2*p3)/p1
            if(i>=1)
            cat("MCWBRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=9","\n")
          }
            if((v+2-2*p2-2*p3)%%p1==0)
            {
              i=(v+2-2*p2-2*p3)/p1
              if(i>=1)
              cat("MCWBRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=9","\n")
            }            
            
              
  if((v+3-p2-p3)%%p1==0)
  {
  i=(v+3-p2-p3)/p1
  if(i>=1)
  cat("MCBGRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=10","\n")
  }
  if((v+3-p2-2*p3)%%p1==0)
  {
  i=(v+3-p2-2*p3)/p1
  if(i>=1)
   cat("MCBGRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=10","\n")
   }
   if((v+3-2*p2-p3)%%p1==0)
   {
   i=(v+3-2*p2-p3)/p1
   if(i>=1)
   cat("MCBGRMD-O2 IS POSSIBLE with two sets of p2 and one sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=10","\n")
   }
   if((v+3-2*p2-2*p3)%%p1==0)
   {
   i=(v+3-2*p2-2*p3)/p1
   if(i>=1)
   cat("MCBGRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=10","\n")
   }              
            
      #cat("No Design IS POSSIBLE IN three different PERIOD SIZES FOR GIVEN", "V=",v, "P1=",p1, "P2=",p2, "AND", "P3=",p3)
  }else
  
  
  if(v%%2==0){
    
    if((v-2-p2-p3)%%p1==0)
    {
      i=(v-2-p2-p3)/p1
      if(i>=1)
      cat("MCPRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=11","\n")
    }
      if((v-2-p2-2*p3)%%p1==0)
      {
        i=(v-2-p2-2*p3)/p1
        if(i>=1)
        cat("MCPRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=11","\n")
      }
        if((v-2-2*p2-p3)%%p1==0)
        {
          i=(v-2-2*p2-2*p3)/p1
          if(i>=1)
          cat("MCPRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=11","\n")
        }
          if((v-2-2*p2-2*p3)%%p1==0)
          {
            i=(v-2-2*p2-2*p3)/p1
            if(i>=1)
            cat("MCPRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=11","\n")
          }
            
        
            if((v-1-p2-p3)%%p1==0)
            {
              i=(v-1-p2-p3)/p1
              if(i>=1)
              cat("MCSPRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=12","\n")
            }
              if((v-1-p2-2*p3)%%p1==0)
              {
                i=(v-1-p2-2*p3)/p1
                if(i>=1)
                cat("MCSPRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=12","\n")
              }
                if((v-1-2*p2-p3)%%p1==0)
                {
                  i=(v-1-2*p2-p3)/p1
                  if(i>=1)
                  cat("MCSPRMD-O1 IS POSSIBLE with two sets of p2 and one sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=12","\n")
                }
                  if((v-1-2*p2-2*p3)%%p1==0)
                  {
                    i=(v-1-2*p2-2*p3)/p1
                    if(i>=1)
                    cat("MCSPRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=12","\n")
                  }
  
                              
  if((v-3-p2-p3)%%p1==0)
  {
  i=(v-3-p2-p3)/p1
  if(i>=1)
  cat("MCPRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=13","\n")
  }
  if((v-3-p2-2*p3)%%p1==0)
  {
  i=(v-3-p2-2*p3)/p1
  if(i>=1)
  cat("MCPRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=13","\n")
  }
  if((v-3-2*p2-p3)%%p1==0)
  {
  i=(v-3-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCPRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=13","\n")
  }
  if((v-3-2*p2-2*p3)%%p1==0)
  {
  i=(v-3-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCPRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=13","\n")
  }
                  
    if((v-2-p2-p3)%%p1==0)
    {
      i=(v-2-p2-p3)/p1
      if(i>=1)
      cat("MCSPRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=14","\n")
    }
      if((v-2-p2-2*p3)%%p1==0)
      {
        i=(v-2-p2-2*p3)/p1
        if(i>=1)
        cat("MCSPRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=14","\n")
      }
        if((v-2-2*p2-p3)%%p1==0)
        {
          i=(v-2-2*p2-2*p3)/p1
          if(i>=1)
          cat("MCSPRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=14","\n")
        }
          if((v-2-2*p2-2*p3)%%p1==0)
          {
            i=(v-2-2*p2-2*p3)/p1
            if(i>=1)
            cat("MCSPRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=14","\n")
          }
                    
            if((v-4-p2-p3)%%p1==0)
            {
              i=(v-4-p2-p3)/p1
              if(i>=1)
              cat("MCPRMD-EII IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=15","\n")
            }
              if((v-4-p2-2*p3)%%p1==0)
              {
                i=(v-4-p2-2*p3)/p1
                if(i>=1)
                cat("MCPRMD-EII IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=15","\n")
              }
                if((v-4-2*p2-p3)%%p1==0)
                {
                  i=(v-4-2*p2-2*p3)/p1
                  if(i>=1)
                  cat("MCPRMD-EII IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=15","\n")
                }
                  if((v-4-2*p2-2*p3)%%p1==0)
                  {
                    i=(v-4-2*p2-2*p3)/p1
                    if(i>=1)
                    cat("MCPRMD-EII IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=15","\n")
}
                        
  if((v-3-p2-p3)%%p1==0)
  {
  i=(v-3-p2-p3)/p1
  if(i>=1)
  cat("MCSPBRMD-EII IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=16","\n")
  }
  if((v-3-p2-2*p3)%%p1==0)
  {
  i=(v-3-p2-2*p3)/p1
  if(i>=1)
  cat("MCSPBRMD-EII IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=16","\n")
  }
  if((v-3-2*p2-p3)%%p1==0)
  {
  i=(v-3-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCSPBRMD-EII IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=16","\n")
  }
  if((v-3-2*p2-2*p3)%%p1==0)
  {
  i=(v-3-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCSPBRMD-EII IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=16","\n")
 }
   
                            
   if((v-p2-p3)%%p1==0)
   {
     i=(v-p2-p3)/p1
     if(i>=1)
     cat("MCWBRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=17","\n")
   }
     if((v-p2-2*p3)%%p1==0)
     {
     i=(v-p2-2*p3)/p1
    if(i>=1)
    cat("MCWBRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=17","\n")
    }
       if((v-2*p2-p3)%%p1==0)
       {
         i=(v-2*p2-2*p3)/p1
         if(i>=1)
         cat("MCWBRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=17","\n")
       }
         if((v-2*p2-2*p3)%%p1==0)
         {
           i=(v-2*p2-2*p3)/p1
           if(i>=1)
           cat("MCWBRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=17","\n")
         }
 
           
           if((v+1-p2-p3)%%p1==0)
           {
             i=(v+1-p2-p3)/p1
             if(i>=1)
             cat("MCBGRMD-O1 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=18","\n")
           }
             if((v+1-p2-2*p3)%%p1==0)
             {
               i=(v+1-p2-2*p3)/p1
               if(i>=1)
               cat("MCBGRMD-O1 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=18","\n")
             }
               if((v-2*p2-p3)%%p1==0)
               {
                 i=(v+1-2*p2-2*p3)/p1
                 if(i>=1)
                 cat("MCBGRMD-O1 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=18","\n")
               }
                 if((v+1-2*p2-2*p3)%%p1==0)
                 {
                   i=(v+1-2*p2-2*p3)/p1
                   if(i>=1)
                   cat("MCBGRMD-O1 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=18","\n")
                 }
                   
                   
  if((v+1-p2-p3)%%p1==0)
  {
  i=(v+1-p2-p3)/p1
  if(i>=1)
   cat("MCWBRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=19","\n")
  }
  if((v+1-p2-2*p3)%%p1==0)
  {
  i=(v+1-p2-2*p3)/p1
  if(i>=1)
  cat("MCWBRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=19","\n")
  }
  if((v+1-2*p2-p3)%%p1==0)
  {
  i=(v+1-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCWBRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=19","\n")
  }
  if((v+1-2*p2-2*p3)%%p1==0)
  {
  i=(v+1-2*p2-2*p3)/p1
  if(i>=1)
  cat("MCWBRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=19","\n")
  }
    
                                 
    if((v+2-p2-p3)%%p1==0)
    {
      i=(v+2-p2-p3)/p1
      if(i>=1)
      cat("MCSBGRMD-O2 IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=20","\n")
    }
      if((v+2-p2-2*p3)%%p1==0)
      {
        i=(v+2-p2-2*p3)/p1
        if(i>=1)
        cat("MCSBGRMD-O2 IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=20","\n")
      }
        if((v+2-2*p2-p3)%%p1==0)
        {
          i=(v+2-2*p2-2*p3)/p1
          if(i>=1)
          cat("MCSBGRMD-O2 IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=20","\n")
        }
          if((v+2-2*p2-2*p3)%%p1==0)
          {
            i=(v+2-2*p2-2*p3)/p1
            if(i>=1)
            cat("MCSBGRMD-O2 IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=20","\n")
          }
            
            
                                        
            if((v+2-p2-p3)%%p1==0)
            {
              i=(v+2-p2-p3)/p1
              if(i>=1)
              cat("MCWBRMD-EII IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=21","\n")
            }
              if((v+2-p2-2*p3)%%p1==0)
              {
                i=(v+2-p2-2*p3)/p1
                if(i>=1)
                cat("MCWBRMD-EII IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=21","\n")
              }
                if((v+2-2*p2-p3)%%p1==0)
                {
                  i=(v+2-2*p2-2*p3)/p1
                  if(i>=1)
                  cat("MCWBRMD-EII IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=21","\n")
                }
                  if((v+2-2*p2-2*p3)%%p1==0)
                  {
                    i=(v+2-2*p2-2*p3)/p1
                    if(i>=1)
                    cat("MCWBRMD-EII IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=21","\n")
                  }
                         
                    
                                       
                    if((v+3-p2-p3)%%p1==0)
                    {
                      i=(v+3-p2-p3)/p1
                      if(i>=1)
                      cat("MCSBGRMD-EII IS POSSIBLE with one set of p2 and p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=22","\n")
                    }
                      if((v+3-p2-2*p3)%%p1==0)
                      {
                        i=(v+3-p2-2*p3)/p1
                        if(i>=1)
                        cat("MCSBGRMD-EII IS POSSIBLE with one set of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=22","\n")
                      }
                        if((v+3-2*p2-p3)%%p1==0)
                        {
                          i=(v+3-2*p2-2*p3)/p1
                          if(i>=1)
                          cat("MCSBGRMD-EII IS POSSIBLE with two sets of p2 and two sets of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=22")
                        }
                          if((v+3-2*p2-2*p3)%%p1==0)
                          {
                            i=(v+3-2*p2-2*p3)/p1
                            if(i>=1)
                            cat("MCSBGRMD-EII IS POSSIBLE with two sets of p2 and two set of p3", "V=",v, "P1=",p1, "P2=",p2, "P3=",p3, "AND", "i=",i, "AND ALSO RUN D=22","\n")
                          }
                                                    
  cat("No Design IS POSSIBLE IN EQUAL PERIOD SIZES FOR GIVEN", "V=",v,  "AND", "P1=",p1, "P2=",p2,"P3=",p3)
    
  }
}




#################################################################################
# Selection of i group of size p1 from adjusted A. The set of remaining 
# (Unselected) elements are saved in object B2. 
#################################################################################

grouping1<-function(A,p,v,i){
  bs<-c()
  z=0;f=1
  A1=A
  while(f<=i){
    
    for(y in 1:5000){
      comp<-sample(1:length(A1),p)
      com<-A1[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs<-rbind(bs,com)
        A1<-A1[-comp]
        z<-z+1
        f=f+1
      }
      if(z==i) break
    }
    if(z<i) {bs<-c();z=1;f=2;A1=A}  
  }
  list(B1=bs,B2=A1)
}

#################################################################################
# Selection of i group of size p1 from adjusted A and selection of required 
# number of groups of size p2 from B2. The set of remaining (Unselected) 
# elements are saved in B3.
#################################################################################

grouping2<-function(A,p,v,i,sp2){
  bs1<-c()
  j=i+sp2
  z=0;f=1
  A1=A
  while(f<=j){
    s<-grouping1(A1,p[1],v,i)
    A2<-s$B2
    z=i;f=f+i
    for(y in 1:1000){
      comp<-sample(1:length(A2),p[2])
      com<-A2[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs1<-rbind(bs1,com)
        A2<-A2[-comp]
        z<-z+1
        f=f+1
      }
      if(z==j) break
    }
    
    
    if(z<j) {bs1<-c();z=1;f=2;A1=A}  
    
  }
  
  list(B1=s$B1,B2=bs1,B3=A2)
}

#################################################################################
# Selection of i group of size p1 from adjusted A, selection of required number 
# of groups of size p2 from B2 and division of required number of groups of size 
# p3 from B3.
#################################################################################
grouping3<-function(A,p,v,i,sp2,sp3){
  bs1<-c()
  j=i+sp2+sp3
  z=0;f=1
  A1=A
  while(f<=j){
    s<-grouping2(A1,p,v,i,sp2)
    A3<-s$B3
    z=i+sp2;f=f+i+sp2
    if(j-z==1){A3<-c(0,A3)}
    for(y in 1:1000){
      comp<-sample(1:length(A3),p[3])
      com<-A3[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs1<-rbind(bs1,com)
        A3<-A3[-comp]
        z<-z+1
        f=f+1
        if(j-z==1){A3=c(0,A3)}
      }
      if(z==j) break
    }
    if(z<j) {bs1<-c();z=1;f=2;A1=A}  
  }
  
  gs1<-t(apply(s$B1,1,sort))
  gs1<-cbind(gs1,rowSums(gs1),rowSums(gs1)/v)
  rownames(gs1)<-paste("G",1:i, sep="")
  colnames(gs1)<-c(paste(1:p[1], sep=""),"sum" ,"sum/v")
  
  gs2<-t(apply(s$B2,1,sort))
  gs2<-cbind(gs2,rowSums(gs2),rowSums(gs2)/v)
  rownames(gs2)<-paste("G",(i+1):(i+sp2), sep="")
  colnames(gs2)<-c(paste(1:p[2], sep=""),"sum" ,"sum/v")
  
  
  gs3<-t(apply(bs1,1,sort))
  gs3<-cbind(gs3,rowSums(gs3),rowSums(gs3)/v)
  rownames(gs3)<-paste("G",(i+sp2+1):(i+sp2+sp3), sep="")
  colnames(gs3)<-c(paste(1:p[3], sep=""),"sum" ,"sum/v")
  
  
  fs1<-t(apply(s$B1,1,sort))
  fs1<-delmin(fs1)
  rownames(fs1)<-paste("S",1:i, sep="")
  colnames(fs1)<-rep("",(p[1])-1)
  
  fs2<-t(apply(s$B2,1,sort))
  fs2<-delmin(fs2)
  rownames(fs2)<-paste("S",(i+1):(i+sp2), sep="")
  colnames(fs2)<-rep("",(p[2])-1)
  
  
  fs3<-t(apply(bs1,1,sort))
  fs3<-delmin(fs3)
  rownames(fs3)<-paste("S",(i+sp2+1):(i+sp2+sp3), sep="")
  colnames(fs3)<-rep("",(p[3]-1))
  
  
  list(B1=list(fs1,fs2,fs3),B4=list(gs1,gs2,gs3),B5=A3)
}

#######################################################################
# Obtaining set(s) of shifts by deleting smallest value of each group
#######################################################################

delmin<-function(z){
  fs<-c()
  n<-nrow(z)
  c<-ncol(z)-1
  for(i in 1:n){
    z1<-z[i,]
    z2<-z1[z1!=min(z1)]
    fs<-rbind(fs,z2)
  }
  return(fs)
}

#################################################################################
# Selection of adjusted A and the set(s) of shifs to obtain Circular Balanced, Strongly Balanced and Weakly Balanced 
# Repeated Measurements Designs for period of three different sizes. 
##################################################################################
# D=1: Minimal CBRMDs 
# D=2: Minimal CSBRMDs 
# D=3: Minimal CPBRMDs
# D=4: Minimal CSPBRMDs 
# D=5: Minimal CWBRMDs
# D=6: Minimal CSBGRMDs 

 
#   p: Vector of three different period sizes 
#   i: Number of sets of shifts for p1
# Sp2: Number of sets of shifts for p2
# Sp3: Number of sets of shifts for p3


CGSBRMD_3diffsize<-function(v,p,i,D,sp2,sp3){
  if(length(p)>3 | length(p)<3){stop("Must be length(p)=3")}
  if(any(p<=2)!=0) stop("p=Period sizes: Each period size must be greater than 2")
  if(i<=0) stop("i=must be a positive integer")
  if(p[1]==p[2] | p[2]==p[3] |  p[1]==p[3]  ) stop("Must be fullfill this condition:period size are not equal")
  
  setClass( "stat_test", representation("list"))
  
  setMethod("show", "stat_test", function(object) {
    row <- paste(rep("=", 52), collapse = "")
    cat(row, "\n")
    cat("Following are required sets of shifts to obtain the 
minimal class for", "v=" ,object$R[1], ",","p1=",object$R[2],",","p2=",object$R[3],"and","p3=",object$R[4],"\n")
    row <- paste(rep("=", 52), collapse = "")
    cat(row, "\n")
    print(object$S[[1]])
    cat("\n")
    print(object$S[[2]])
    cat("\n")
    print(object$S[[3]])
  })
  
  
  
  if((sp2==1)&(sp3==1)){
    if(v%%2!=0){  
      if(D==1){ 
        v=p[1]*i+p[2]+p[3]+1
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==2){ 
        v=p[1]*i+p[2]+p[3]
        A<-c(0,1:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==3){ 
        v=p[1]*i+p[2]+p[3]+3
        A<-c(1,2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSPBRMD-O1
      if(D==4){ 
        v=p[1]*i+p[2]+p[3]+2
        A<-c(0, 1, 2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCPBRMD-O2
      if(D==5){ 
        v=p[1]*i+p[2]+p[3]+4
        A<-c(3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMD-O2
      if (D==6){ 
        v=p[1]*i+p[2]+p[3]+3
        A<-c(0,3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-O1
      if(D==7){ 
        v=p[1]*i+p[2]+p[3]-1
        A<-c(1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMD-O1 
      if( D==8){ 
        v=p[1]*i+p[2]+p[3]-2
        A<-c(0, 1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-02
      if(D==9){ 
        v=p[1]*i+p[2]+p[3]-2
        A<-c(1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSBGRMD-O2 
      if(D==10){ 
        v=p[1]*i+p[2]+p[3]-3
        A<-c(0,1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
    if(v%%2==0){
      if(D==11){ 
        v=p[1]*i+p[2]+p[3]+2
        A<-c(1,2:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==12){ 
        v=p[1]*i+p[2]+p[3]+1
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==13){ 
        v=p[1]*i+p[2]+p[3]+3
        A<-c(1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMDs-II 
      if(D==14){ 
        v=p[1]*i+p[2]+p[3]+2
        A<-c(0,1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      if(D==15){ 
        v=p[1]*i+p[2]+p[3]+4
        A<-c(1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==16){ 
        v=p[1]*i+p[2]+p[3]+3
        A<-c(0,1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==17){ 
        v=p[1]*i+p[2]+p[3]
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==18){ 
        v=p[1]*i+p[2]+p[3]-1
        A<-c(0,1:(v-1),(v-2/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMDs-II
      if(D==19){ 
        v=p[1]*i+p[2]+p[3]-1
        A<-c(1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMDs-II 
      if(D==20){ 
        v=p[1]*i+p[2]+p[3]-2
        A<-c(0,1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==21){ 
        v=p[1]*i+p[2]+p[3]-2
        A<-c(1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==22){ 
        v=p[1]*i+p[2]+p[3]-3
        A<-c(0,1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
  }
  if((sp2==2)&(sp3==1)){
    #MCPBRMD-O1
    if(v%%2!=0){  
      if(D==1){ 
        v=p[1]*i+2*p[2]+p[3]+1
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==2){ 
        v=p[1]*i+2*p[2]+p[3]
        A<-c(0,1:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==3){ 
        v=p[1]*i+2*p[2]+p[3]+3
        A<-c(1,2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSPBRMD-O1
      if(D==4){ 
        v=p[1]*i+2*p[2]+p[3]+2
        A<-c(0, 1, 2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCPBRMD-O2
      if(D==5){ 
        v=p[1]*i+2*p[2]+p[3]+4
        A<-c(3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMD-O2
      if (D==6){ 
        v=p[1]*i+2*p[2]+p[3]+3
        A<-c(0,3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-O1
      if(D==7){ 
        v=p[1]*i+2*p[2]+p[3]-1
        A<-c(1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMD-O1 
      if( D==8){ 
        v=p[1]*i+2*p[2]+p[3]-2
        A<-c(0, 1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-02
      if(D==9){ 
        v=p[1]*i+2*p[2]+p[3]-2
        A<-c(1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSBGRMD-O2 
      if(D==10){ 
        v=p[1]*i+2*p[2]+p[3]-3
        A<-c(0,1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
    if(v%%2==0){
      if(D==11){ 
        v=p[1]*i+2*p[2]+p[3]+2
        A<-c(1,2:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==12){ 
        v=p[1]*i+2*p[2]+p[3]+1
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==13){ 
        v=p[1]*i+2*p[2]+p[3]+3
        A<-c(1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMDs-II 
      if(D==14){ 
        v=p[1]*i+2*p[2]+p[3]+2
        A<-c(0,1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      if(D==15){ 
        v=p[1]*i+2*p[2]+p[3]+4
        A<-c(1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==16){ 
        v=p[1]*i+2*p[2]+p[3]+3
        A<-c(0,1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==17){ 
        v=p[1]*i+2*p[2]+p[3]
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==18){ 
        v=p[1]*i+2*p[2]+p[3]-1
        A<-c(0,1:(v-1),(v-2/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMDs-II
      if(D==19){ 
        v=p[1]*i+2*p[2]+p[3]-1
        A<-c(1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMDs-II 
      if(D==20){ 
        v=p[1]*i+2*p[2]+p[3]-2
        A<-c(0,1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==21){ 
        v=p[1]*i+2*p[2]+p[3]-2
        A<-c(1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==22){ 
        v=p[1]*i+2*p[2]+p[3]-3
        A<-c(0,1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
  } 
  if((sp2==1)&(sp3==2)){
    if(v%%2!=0){  
      if(D==1){ 
        v=p[1]*i+p[2]+2*p[3]+1
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==2){ 
        v=p[1]*i+p[2]+2*p[3]
        A<-c(0,1:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==3){ 
        v=p[1]*i+p[2]+2*p[3]+3
        A<-c(1,2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSPBRMD-O1
      if(D==4){ 
        v=p[1]*i+p[2]+2*p[3]+2
        A<-c(0, 1, 2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCPBRMD-O2
      if(D==5){ 
        v=p[1]*i+p[2]+2*p[3]+4
        A<-c(3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMD-O2
      if (D==6){ 
        v=p[1]*i+p[2]+2*p[3]+3
        A<-c(0,3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-O1
      if(D==7){ 
        v=p[1]*i+p[2]+2*p[3]-1
        A<-c(1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMD-O1 
      if( D==8){ 
        v=p[1]*i+p[2]+2*p[3]-2
        A<-c(0, 1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-02
      if(D==9){ 
        v=p[1]*i+p[2]+2*p[3]-2
        A<-c(1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSBGRMD-O2 
      if(D==10){ 
        v=p[1]*i+p[2]+2*p[3]-3
        A<-c(0,1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
    if(v%%2==0){
      if(D==11){ 
        v=p[1]*i+p[2]+2*p[3]+2
        A<-c(1,2:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==12){ 
        v=p[1]*i+p[2]+2*p[3]+1
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==13){ 
        v=p[1]*i+p[2]+2*p[3]+3
        A<-c(1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMDs-II 
      if(D==14){ 
        v=p[1]*i+p[2]+2*p[3]+2
        A<-c(0,1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      if(D==15){ 
        v=p[1]*i+p[2]+2*p[3]+4
        A<-c(1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==16){ 
        v=p[1]*i+p[2]+2*p[3]+3
        A<-c(0,1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==17){ 
        v=p[1]*i+p[2]+2*p[3]
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==18){ 
        v=p[1]*i+p[2]+2*p[3]-1
        A<-c(0,1:(v-1),(v-2/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMDs-II
      if(D==19){ 
        v=p[1]*i+p[2]+2*p[3]-1
        A<-c(1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMDs-II 
      if(D==20){ 
        v=p[1]*i+p[2]+2*p[3]-2
        A<-c(0,1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==21){ 
        v=p[1]*i+p[2]+2*p[3]-2
        A<-c(1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==22){ 
        v=p[1]*i+p[2]+2*p[3]-3
        A<-c(0,1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
  }
  
  if((sp2==2)&(sp3==2)){
    if(v%%2!=0){  
      if(D==1){ 
        v=p[1]*i+2*p[2]+2*p[3]+1
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==2){ 
        v=p[1]*i+2*p[2]+2*p[3]
        A<-c(0,1:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==3){ 
        v=p[1]*i+2*p[2]+2*p[3]+3
        A<-c(1,2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSPBRMD-O1
      if(D==4){ 
        v=p[1]*i+2*p[2]+2*p[3]+2
        A<-c(0, 1, 2:((v-3)/2), ((v+3)/2), ((v+5)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCPBRMD-O2
      if(D==5){ 
        v=p[1]*i+2*p[2]+2*p[3]+4
        A<-c(3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMD-O2
      if (D==6){ 
        v=p[1]*i+2*p[2]+2*p[3]+3
        A<-c(0,3,4:(v-4), (v-2), (v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-O1
      if(D==7){ 
        v=p[1]*i+2*p[2]+2*p[3]-1
        A<-c(1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMD-O1 
      if( D==8){ 
        v=p[1]*i+2*p[2]+2*p[3]-2
        A<-c(0, 1,2:(v-1), ((v-1)/2), ((v+1)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMD-02
      if(D==9){ 
        v=p[1]*i+2*p[2]+2*p[3]-2
        A<-c(1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      #MCSBGRMD-O2 
      if(D==10){ 
        v=p[1]*i+2*p[2]+2*p[3]-3
        A<-c(0,1,2:(v-1),1,2,(v-3))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
    if(v%%2==0){
      if(D==11){ 
        v=p[1]*i+2*p[2]+2*p[3]+2
        A<-c(1,2:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==12){ 
        v=p[1]*i+2*p[2]+2*p[3]+1
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==13){ 
        v=p[1]*i+2*p[2]+2*p[3]+3
        A<-c(1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSPBRMDs-II 
      if(D==14){ 
        v=p[1]*i+2*p[2]+2*p[3]+2
        A<-c(0,1,2:((v-4)/2),(v/2),((v+2)/2),((v+4)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      
      if(D==15){ 
        v=p[1]*i+2*p[2]+2*p[3]+4
        A<-c(1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==16){ 
        v=p[1]*i+2*p[2]+2*p[3]+3
        A<-c(0,1,2:((v-4)/2),((v+4)/2),((v+6)/2):(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==17){ 
        v=p[1]*i+2*p[2]+2*p[3]
        A<-c(1,2:(v-1))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==18){ 
        v=p[1]*i+2*p[2]+2*p[3]-1
        A<-c(0,1:(v-1),(v-2/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCWBRMDs-II
      if(D==19){ 
        v=p[1]*i+2*p[2]+2*p[3]-1
        A<-c(1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      #MCSBGRMDs-II 
      if(D==20){ 
        v=p[1]*i+2*p[2]+2*p[3]-2
        A<-c(0,1,2:(v-1), 1,((v-2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==21){ 
        v=p[1]*i+2*p[2]+2*p[3]-2
        A<-c(1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(D==22){ 
        v=p[1]*i+2*p[2]+2*p[3]-3
        A<-c(0,1,2:(v-1),((v-2)/2),(v/2),((v+2)/2))
        A1<-grouping3(A,p,v,i,sp2,sp3)
        A2<-c(v,p);names(A2)<-c("V","p1","p2","p3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
  } 
  
  new("stat_test", x) 
}


################################################################################
# Generation of design using sets of cyclical shifts
################################################################################
# H is an output object from CGSBRMD_3diffsize
# The output is called using the design_CGSBRMD to generate design
design_CGSBRMD<-function(H){
  
  setClass( "CGSBRMD_design", representation("list"))
  setMethod("show", "CGSBRMD_design", function(object) {
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    cat("Following is minimal CGSBRMD for", "v=" ,object$R[1], "and","p=",object$R[2], "\n")
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    for(i in 1:length(ss)){
      W<-ss[[i]]
      nr<-dim(W)[1]
      for(j in 1:nr){
        print(object$Design[[i]][[j]])
        cat("\n\n")
      }}
  })  
  
  v<-H$R[1]
  p<-H$R[2]
  ss<-H$S  
  treat<-(1:v)-1
  fn<-(1:v)
  G<-list()
  
  
  for(j in 1:length(ss)){ 
    W<-ss[[j]]
    nr<-dim(W)[1]
    nc<-dim(W)[2]
    D<-list()
    
    for(i in 1:nr){
      dd<-c()
      d1<-matrix(treat,(nc+1),v,byrow = T)
      ss1<-cumsum(c(0,W[i,]))
      dd2<-d1+ss1
      dd<-rbind(dd,dd2)
      rr<-dd[which(dd>=v)]%%v
      dd[which(dd>=v)]<-rr
      colnames(dd)<-paste("B",fn, sep="")
      rownames(dd)<-rep("",(nc+1))
      fn<-fn+v
      D[[i]]<-dd
    }
    G[[j]]<-D
    
  }
  
  x<-list(Design=G,R=H$R)
  new("CGSBRMD_design", x)
}

###############################################################################
# Examples: Using MCBRMD,MCSBRMD,MCPBRMD-I,MCPBRMD-II,MCWBRMD-I,
#MCSBGRMD-I,MCSPBRMD-I and MCSPBRMD-II_equalsize function to obtain the set(s) of shifts
# for construction of Circular  Repeated Measure 
# designs for period of three different sizes (p1, p2 and p3)
###############################################################################


#example
(H<-Designcheck(v=27,p1=7,p2=6,p3=5))


# Examples for Case#1
p=c(13,9,7);i=5;D=1;sp2=1;sp3=1
(H<-CGSBRMD_3diffsize(p,i=5,D=1,sp2,sp3))
(H<-CGSBRMD_3diffsize(p=c(7,6,5),v=27,i=2,D=10,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(8,9,10),v=40,i=7,D=5,sp2=2,sp3=2))
H$G
(H<-CGSBRMD_3diffsize(p=c(8,5,3),v=3,i=5,D=1,sp2=1,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(20,10,8),v=2,i=8,D=1,sp2=1,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(30,13,10),i=5,D=2,sp2=1,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(11,9,7),v=3,i=4,D=1,sp2=1,sp3=1))




# Examples for Case#2
(H<-CGSBRMD_3diffsize(p=c(13,9,6),i=4,D=2,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(33,21,10),i=9,D=1,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(13,10,9),i=8,D=2,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(41,10,8),i=5,D=1,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(30,20,15),i=5,D=2,sp2=2,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(31,13,10),i=4,D=1,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(31,10,9),i=7,D=1,sp2=2,sp3=1))


# Examples for Case#3
p=c(13,9,7);i=4;D=2;sp2=1;sp3=2
(H<-CGSBRMD_3diffsize(p=c(13,8,7),v=48,i=2,D=2,sp2=1,sp3=2))
H$G
(H<-CGSBRMD_3diffsize(p=c(33,21,10),i=9,D=1,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(13,10,9),i=8,D=2,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(41,10,8),i=5,D=1,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(30,20,15),i=5,D=2,sp2=1,sp3=2))

(H<-CGSBRMD_3diffsize(p=c(31,13,10),i=4,D=1,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(31,10,9),i=7,D=1,sp2=1,sp3=2))


# Examples for Case#4
(H<-CGSBRMD_3diffsize(p=c(13,5,3),v=68,i=4,D=2,sp2=2,sp3=2))
H$G
(H<-CGSBRMD_3diffsize(p=c(11,8,4),i=5,D=1,sp2=2,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(11,8,5),i=7,D=1,sp2=2,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(11,8,6),i=9,D=1,sp2=2,sp3=2))

(H<-CGSBRMD_3diffsize(p=c(9,8,6),v=83,i=6,D=1,sp2=2,sp3=2))






# Examples for Case#1
p=c(13,9,7);i=5;D=1;sp2=1;sp3=1
(H<-CGSBRMD_3diffsize(p,i=5,D=3,sp2,sp3))
(H<-CGSBRMD_3diffsize(p=c(13,8,4),v=64,i=4,D=4,sp2=1,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(6,5,3),v=38,i=5,D=4,sp2=1,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(6,5,3),v=40,i=5,D=3,sp2=1,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(20,10,8),v=2,i=8,D=3,sp2=1,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(30,13,10),i=5,D=4,sp2=1,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(31,13,9),i=4,D=3,sp2=1,sp3=1))




# Examples for Case#2
(H<-CGSBRMD_3diffsize(p=c(13,9,6),i=4,D=4,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(33,21,10),i=9,D=3,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(13,10,9),i=8,D=4,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(41,10,8),i=5,D=3,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(30,20,15),i=5,D=4,sp2=2,sp3=1))

(H<-CGSBRMD_3diffsize(p=c(31,13,10),i=4,D=3,sp2=2,sp3=1))
(H<-CGSBRMD_3diffsize(p=c(31,10,9),i=7,D=3,sp2=2,sp3=1))


# Examples for Case#3
p=c(13,9,7);i=4;D=2;sp2=1;sp3=2
(H<-CGSBRMD_3diffsize(p=c(13,8,7),v=48,i=2,D=4,sp2=1,sp3=2))
H$G
(H<-CGSBRMD_3diffsize(p=c(33,21,10),i=9,D=3,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(13,10,9),i=8,D=4,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(41,10,8),i=5,D=3,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(30,20,15),i=5,D=4,sp2=1,sp3=2))

(H<-CGSBRMD_3diffsize(p=c(31,13,10),i=4,D=3,sp2=1,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(31,10,9),i=7,D=3,sp2=1,sp3=2))


# Examples for Case#4
(H<-CGSBRMD_3diffsize(p=c(13,5,3),v=68,i=2,D=4,sp2=2,sp3=2))
H$G
(H<-CGSBRMD_3diffsize(p=c(11,8,4),i=5,D=3,sp2=2,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(11,8,5),i=7,D=3,sp2=2,sp3=2))
(H<-CGSBRMD_3diffsize(p=c(11,8,6),i=9,D=3,sp2=2,sp3=2))

(H<-CGSBRMD_3diffsize(p=c(12,8,6),i=9,D=3,sp2=2,sp3=2))







