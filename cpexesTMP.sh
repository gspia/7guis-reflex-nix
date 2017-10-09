
if [ -d "./bin" ];
then 
  find . -path "./dist*" -executable -type f -exec cp '{}' ./bin/ \; 
else 
  echo "Couldn't find ./bin-directory. Make sure it exists." 
fi

