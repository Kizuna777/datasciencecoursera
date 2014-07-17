### There will be an object called 'iris' in your workspace. In this dataset, what is the mean of 'Sepal.Length' for the species virginica? (Please only enter the numeric result and nothing else.)

* tapply(iris$Sepal.Length,iris$Species, mean)

### Continuing with the 'mtcars' dataset from the previous Question, what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
hp<-function (x,y,z) {
    l=tapply(x$hp,mtcars$cyl==y,mean)
    r=tapply(x$hp,mtcars$cyl==z,mean)
    e=l-r
    t=e[TRUE]
print(t)

}