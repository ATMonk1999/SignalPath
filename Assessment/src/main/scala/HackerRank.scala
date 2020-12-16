import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Util {
  def time[R](f: => R): R = {
    val t0 = System.nanoTime()
    val result = f
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000 + "us")
    result
  }

  def getResult[T](f: => T): String = {
    Try(f) match {
      case Success(value) => value.toString
      case Failure(exception) => exception.getMessage
    }
  }
}

object MinimumBribes {
  import Util._

  // Complete the minimumBribes function below.
  def minimumBribes(q: Array[Int]): Int = {
    @tailrec
    def getBribes(saved: Int, pos: Int, bribes: Int): Int = {
      if (pos <= 0) bribes
      else {
        val cur = q(pos - 1)
        val delta = cur - pos
        if(delta > 2) throw new Exception("Too chaotic")
        val (nextBribes, nextSaved) = {
          if (delta > 0) (bribes + delta, saved)
          else if (cur > saved) (bribes + 1, saved)
          else if (delta < 0) (bribes, cur)
          else (bribes, saved)
        }
        getBribes(nextSaved, pos - 1, nextBribes)
      }
    }

    getBribes(q.length + 1, q.length, bribes = 0)
  }

  def apply() {
    def runTest(fn: Array[Int] => Int, test: Array[Int]): Unit = {
      println(s"${test.mkString(",")}\nbribes: ${getResult(fn(test))}")
    }

    time(runTest(minimumBribes, Array(2, 1, 5, 3, 4)))
    println

    time(runTest(minimumBribes, Array(1, 2, 5, 3, 7, 8, 6, 4)))
    println

    time(runTest(minimumBribes, Array(1, 2, 5, 3, 4, 7, 8, 6)))
    println

    val bigTest = Array(
      2, 1, 5, 6, 3, 4, 9, 8, 11, 7, 10, 14, 13, 12, 17, 16, 15, 19, 18,
      22, 20, 24, 23, 21, 27, 28, 25, 26, 30, 29, 33, 32, 31, 35, 36, 34,
      39, 38, 37, 42, 40, 44, 41, 43, 47, 46, 48, 45, 50, 52, 49, 51, 54,
      56, 55, 53, 59, 58, 57, 61, 63, 60, 65, 64, 67, 68, 62, 69, 66, 72,
      70, 74, 73, 71, 77, 75, 79, 78, 81, 82, 80, 76, 85, 84, 83, 86, 89,
      90, 88, 87, 92, 91, 95, 94, 93, 98, 97, 100, 96, 102, 99, 104, 101,
      105, 103, 108, 106, 109, 107, 112, 111, 110, 113, 116, 114, 118, 119,
      117, 115, 122, 121, 120, 124, 123, 127, 125, 126, 130, 129, 128, 131,
      133, 135, 136, 132, 134, 139, 140, 138, 137, 143, 141, 144, 146, 145,
      142, 148, 150, 147, 149, 153, 152, 155, 151, 157, 154, 158, 159, 156,
      161, 160, 164, 165, 163, 167, 166, 162, 170, 171, 172, 168, 169, 175,
      173, 174, 177, 176, 180, 181, 178, 179, 183, 182, 184, 187, 188, 185,
      190, 189, 186, 191, 194, 192, 196, 197, 195, 199, 193, 198, 202, 200,
      204, 205, 203, 207, 206, 201, 210, 209, 211, 208, 214, 215, 216, 212,
      218, 217, 220, 213, 222, 219, 224, 221, 223, 227, 226, 225, 230, 231,
      229, 228, 234, 235, 233, 237, 232, 239, 236, 241, 238, 240, 243, 242,
      246, 245, 248, 249, 250, 247, 244, 253, 252, 251, 256, 255, 258, 254,
      257, 259, 261, 262, 263, 265, 264, 260, 268, 266, 267, 271, 270, 273,
      269, 274, 272, 275, 278, 276, 279, 277, 282, 283, 280, 281, 286, 284,
      288, 287, 290, 289, 285, 293, 291, 292, 296, 294, 298, 297, 299, 295,
      302, 301, 304, 303, 306, 300, 305, 309, 308, 307, 312, 311, 314, 315,
      313, 310, 316, 319, 318, 321, 320, 317, 324, 325, 322, 323, 328, 327,
      330, 326, 332, 331, 329, 335, 334, 333, 336, 338, 337, 341, 340, 339,
      344, 343, 342, 347, 345, 349, 346, 351, 350, 348, 353, 355, 352, 357,
      358, 354, 356, 359, 361, 360, 364, 362, 366, 365, 363, 368, 370, 367,
      371, 372, 369, 374, 373, 376, 375, 378, 379, 377, 382, 381, 383, 380,
      386, 387, 384, 385, 390, 388, 392, 391, 389, 393, 396, 397, 394, 398,
      395, 401, 400, 403, 402, 399, 405, 407, 406, 409, 408, 411, 410, 404,
      413, 412, 415, 417, 416, 414, 420, 419, 422, 421, 418, 424, 426, 423,
      425, 428, 427, 431, 430, 429, 434, 435, 436, 437, 432, 433, 440, 438,
      439, 443, 441, 445, 442, 447, 444, 448, 446, 449, 452, 451, 450, 455,
      453, 454, 457, 456, 460, 459, 458, 463, 462, 464, 461, 467, 465, 466,
      470, 469, 472, 468, 474, 471, 475, 473, 477, 476, 480, 479, 478, 483,
      482, 485, 481, 487, 484, 489, 490, 491, 488, 492, 486, 494, 495, 496,
      498, 493, 500, 499, 497, 502, 504, 501, 503, 507, 506, 505, 509, 511,
      508, 513, 510, 512, 514, 516, 518, 519, 515, 521, 522, 520, 524, 517,
      523, 525, 526, 529, 527, 531, 528, 533, 532, 534, 530, 537, 536, 539,
      535, 541, 538, 540, 543, 544, 542, 547, 548, 545, 549, 546, 552, 550,
      551, 554, 553, 557, 555, 556, 560, 559, 558, 563, 562, 564, 561, 567,
      568, 566, 565, 569, 572, 571, 570, 575, 574, 577, 576, 579, 573, 580,
      578, 583, 581, 584, 582, 587, 586, 585, 590, 589, 588, 593, 594, 592,
      595, 591, 598, 599, 596, 597, 602, 603, 604, 605, 600, 601, 608, 609,
      607, 611, 612, 606, 610, 615, 616, 614, 613, 619, 618, 617, 622, 620,
      624, 621, 626, 625, 623, 628, 627, 631, 630, 633, 629, 635, 632, 637,
      636, 634, 638, 640, 642, 639, 641, 645, 644, 647, 643, 646, 650, 648,
      652, 653, 654, 649, 651, 656, 658, 657, 655, 661, 659, 660, 663, 664,
      666, 662, 668, 667, 670, 665, 671, 673, 669, 672, 676, 677, 674, 679,
      675, 680, 678, 681, 684, 682, 686, 685, 683, 689, 690, 688, 687, 693,
      692, 691, 696, 695, 698, 694, 700, 701, 702, 697, 704, 699, 706, 703,
      705, 709, 707, 711, 712, 710, 708, 713, 716, 715, 714, 718, 720, 721,
      719, 723, 717, 722, 726, 725, 724, 729, 728, 727, 730, 733, 732, 735,
      734, 736, 731, 738, 737, 741, 739, 740, 744, 743, 742, 747, 746, 745,
      750, 748, 752, 749, 753, 751, 756, 754, 758, 755, 757, 761, 760, 759,
      764, 763, 762, 767, 765, 768, 766, 771, 770, 769, 774, 773, 776, 772,
      778, 777, 779, 775, 781, 780, 783, 784, 782, 786, 788, 789, 787, 790,
      785, 793, 791, 792, 796, 795, 794, 798, 797, 801, 799, 803, 800, 805,
      802, 804, 808, 806, 807, 811, 809, 810, 814, 812, 813, 817, 816, 819,
      818, 815, 820, 821, 823, 822, 824, 826, 827, 825, 828, 831, 829, 830,
      834, 833, 836, 832, 837, 839, 838, 841, 835, 840, 844, 842, 846, 845,
      843, 849, 847, 851, 850, 852, 848, 855, 854, 853, 857, 856, 858, 861,
      862, 860, 859, 863, 866, 865, 864, 867, 870, 869, 868, 872, 874, 875,
      871, 873, 877, 878, 876, 880, 881, 879, 884, 883, 885, 882, 888, 886,
      890, 891, 889, 893, 887, 895, 892, 896, 898, 894, 899, 897, 902, 901,
      903, 905, 900, 904, 908, 907, 910, 909, 906, 912, 911, 915, 913, 916,
      918, 914, 919, 921, 917, 923, 920, 924, 922, 927, 925, 929, 928, 926,
      932, 931, 934, 930, 933, 935, 937, 939, 940, 938, 936, 943, 944, 942,
      941, 947, 946, 948, 945, 951, 950, 949, 953, 952, 956, 954, 958, 957,
      955, 961, 962, 963, 959, 964, 966, 960, 965, 969, 968, 971, 967, 970,
      974, 972, 976, 973, 975, 979, 977, 981, 982, 978, 980, 983, 986, 984,
      985, 989, 988, 987, 990, 993, 991, 995, 994, 997, 992, 999, 1000, 996,
      998
    )
    time(runTest(minimumBribes, bigTest))
    println
  }
}

object MinimumSwaps {
  // Complete the minimumSwaps function below.
  def minimumSwaps(arr: Array[Int]): Int = {
    def swap(a: Int, b: Int): Unit = {
      val temp = arr(a)
      arr(a) = arr(b)
      arr(b) = temp
    }

    @tailrec
    def sort(idx: Int, swaps: Int): Int = {
      if(idx == arr.length) swaps
      else {
        val (nextIdx, nextSwaps) = {
          if(arr(idx) == idx + 1) (idx + 1, swaps)
          else {
            swap(idx, arr(idx) - 1)
            (idx, swaps + 1)
          }
        }
        sort(nextIdx, nextSwaps)
      }
    }

    sort(idx = 0, swaps = 0)
  }

  def minimumSwaps2(arr: Array[Int]): Int = {
    def swap(low: Int, high: Int): Unit = {
      val temp = arr(low)
      arr(low) = arr(high)
      arr(high) = temp
    }

    @tailrec
    def merge(left: Int, leftEnd: Int, right: Int, rightEnd: Int, swaps: Int): Int = {
      if(left >= leftEnd || right >= rightEnd) swaps
      else {
        val (nextLeft, nextRight, nextSwaps) = {
          if (arr(left) > leftEnd && arr(right) <= leftEnd) {
            swap(left, right)
            (left + 1, right + 1, swaps + 1)
          }
          else if (arr(left) > leftEnd) (left, right + 1, swaps)
          else if (arr(right) <= leftEnd) (left + 1, right, swaps)
          else (left + 1, right + 1, swaps)
        }
        merge(nextLeft, leftEnd, nextRight, rightEnd, nextSwaps)
      }
    }

    def sort(start: Int, end: Int): Int = {
      val len = end - start
      if(len <= 1) 0
      else if(len == 2) {
        if(arr(start) > arr(start + 1)) {
          swap(start, start + 1)
          1
        }
        else 0
      }
      else {
        val pivot = start + len / 2
        merge(start, pivot, pivot, end, swaps = 0) +
          sort(start, pivot) +
          sort(pivot, end)
      }
    }

    sort(start = 0, end = arr.length)
  }

  def apply(): Unit = {
    import Util._

    def runTest(fn: Array[Int] => Int, test: Array[Int], expected: String): Unit = {
      println(s"${test.mkString(",")}\nswaps: ${getResult(fn(test))}\nexpected: $expected")
    }

    time(runTest(minimumSwaps, Array(7, 1, 3, 2, 4, 5, 6), expected = "5"))
    println

    time(runTest(minimumSwaps, Array(4, 3, 1, 2), expected = "3"))
    println

    time(runTest(minimumSwaps, Array(2, 3, 4, 1, 5), expected = "3"))
    println

    time(runTest(minimumSwaps, Array(1, 3, 5, 2, 4, 6, 7), expected = "3"))
    println

    time(runTest(minimumSwaps, Array(3, 7, 6, 9, 1, 8, 10, 4, 2, 5), expected = "9"))
    println
  }
}

object HackerRank {
  def main(args: Array[String]): Unit = {
//    MinimumBribes()

    MinimumSwaps()
  }
}