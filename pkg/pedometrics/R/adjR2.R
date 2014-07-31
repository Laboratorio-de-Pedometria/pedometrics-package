#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  Purpose        : Get model statistics
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : beta
#  Depends on     : 
#  Dependency of  :
adjR2 <- 
  function (r2, n, p) {
    r2 <- 1 - (1 - r2) * ((n - 1) / (n - p - 1))
    return(r2)
  }
# End!