(() => {
  // node_modules/d3-array/src/ascending.js
  function ascending(a2, b) {
    return a2 == null || b == null ? NaN : a2 < b ? -1 : a2 > b ? 1 : a2 >= b ? 0 : NaN;
  }

  // node_modules/d3-array/src/descending.js
  function descending(a2, b) {
    return a2 == null || b == null ? NaN : b < a2 ? -1 : b > a2 ? 1 : b >= a2 ? 0 : NaN;
  }

  // node_modules/d3-array/src/bisector.js
  function bisector(f) {
    let compare1, compare2, delta;
    if (f.length !== 2) {
      compare1 = ascending;
      compare2 = (d, x2) => ascending(f(d), x2);
      delta = (d, x2) => f(d) - x2;
    } else {
      compare1 = f === ascending || f === descending ? f : zero;
      compare2 = f;
      delta = f;
    }
    function left2(a2, x2, lo = 0, hi = a2.length) {
      if (lo < hi) {
        if (compare1(x2, x2) !== 0)
          return hi;
        do {
          const mid = lo + hi >>> 1;
          if (compare2(a2[mid], x2) < 0)
            lo = mid + 1;
          else
            hi = mid;
        } while (lo < hi);
      }
      return lo;
    }
    function right2(a2, x2, lo = 0, hi = a2.length) {
      if (lo < hi) {
        if (compare1(x2, x2) !== 0)
          return hi;
        do {
          const mid = lo + hi >>> 1;
          if (compare2(a2[mid], x2) <= 0)
            lo = mid + 1;
          else
            hi = mid;
        } while (lo < hi);
      }
      return lo;
    }
    function center2(a2, x2, lo = 0, hi = a2.length) {
      const i = left2(a2, x2, lo, hi - 1);
      return i > lo && delta(a2[i - 1], x2) > -delta(a2[i], x2) ? i - 1 : i;
    }
    return { left: left2, center: center2, right: right2 };
  }
  function zero() {
    return 0;
  }

  // node_modules/d3-array/src/number.js
  function number(x2) {
    return x2 === null ? NaN : +x2;
  }
  function* numbers(values, valueof) {
    if (valueof === void 0) {
      for (let value of values) {
        if (value != null && (value = +value) >= value) {
          yield value;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (value = +value) >= value) {
          yield value;
        }
      }
    }
  }

  // node_modules/d3-array/src/bisect.js
  var ascendingBisect = bisector(ascending);
  var bisectRight = ascendingBisect.right;
  var bisectLeft = ascendingBisect.left;
  var bisectCenter = bisector(number).center;
  var bisect_default = bisectRight;

  // node_modules/d3-array/src/count.js
  function count(values, valueof) {
    let count3 = 0;
    if (valueof === void 0) {
      for (let value of values) {
        if (value != null && (value = +value) >= value) {
          ++count3;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (value = +value) >= value) {
          ++count3;
        }
      }
    }
    return count3;
  }

  // node_modules/d3-array/src/variance.js
  function variance(values, valueof) {
    let count3 = 0;
    let delta;
    let mean2 = 0;
    let sum = 0;
    if (valueof === void 0) {
      for (let value of values) {
        if (value != null && (value = +value) >= value) {
          delta = value - mean2;
          mean2 += delta / ++count3;
          sum += delta * (value - mean2);
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (value = +value) >= value) {
          delta = value - mean2;
          mean2 += delta / ++count3;
          sum += delta * (value - mean2);
        }
      }
    }
    if (count3 > 1)
      return sum / (count3 - 1);
  }

  // node_modules/d3-array/src/deviation.js
  function deviation(values, valueof) {
    const v = variance(values, valueof);
    return v ? Math.sqrt(v) : v;
  }

  // node_modules/d3-array/src/extent.js
  function extent(values, valueof) {
    let min3;
    let max3;
    if (valueof === void 0) {
      for (const value of values) {
        if (value != null) {
          if (min3 === void 0) {
            if (value >= value)
              min3 = max3 = value;
          } else {
            if (min3 > value)
              min3 = value;
            if (max3 < value)
              max3 = value;
          }
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null) {
          if (min3 === void 0) {
            if (value >= value)
              min3 = max3 = value;
          } else {
            if (min3 > value)
              min3 = value;
            if (max3 < value)
              max3 = value;
          }
        }
      }
    }
    return [min3, max3];
  }

  // node_modules/internmap/src/index.js
  var InternMap = class extends Map {
    constructor(entries, key = keyof) {
      super();
      Object.defineProperties(this, { _intern: { value: /* @__PURE__ */ new Map() }, _key: { value: key } });
      if (entries != null)
        for (const [key2, value] of entries)
          this.set(key2, value);
    }
    get(key) {
      return super.get(intern_get(this, key));
    }
    has(key) {
      return super.has(intern_get(this, key));
    }
    set(key, value) {
      return super.set(intern_set(this, key), value);
    }
    delete(key) {
      return super.delete(intern_delete(this, key));
    }
  };
  function intern_get({ _intern, _key }, value) {
    const key = _key(value);
    return _intern.has(key) ? _intern.get(key) : value;
  }
  function intern_set({ _intern, _key }, value) {
    const key = _key(value);
    if (_intern.has(key))
      return _intern.get(key);
    _intern.set(key, value);
    return value;
  }
  function intern_delete({ _intern, _key }, value) {
    const key = _key(value);
    if (_intern.has(key)) {
      value = _intern.get(key);
      _intern.delete(key);
    }
    return value;
  }
  function keyof(value) {
    return value !== null && typeof value === "object" ? value.valueOf() : value;
  }

  // node_modules/d3-array/src/identity.js
  function identity(x2) {
    return x2;
  }

  // node_modules/d3-array/src/sort.js
  function compareDefined(compare = ascending) {
    if (compare === ascending)
      return ascendingDefined;
    if (typeof compare !== "function")
      throw new TypeError("compare is not a function");
    return (a2, b) => {
      const x2 = compare(a2, b);
      if (x2 || x2 === 0)
        return x2;
      return (compare(b, b) === 0) - (compare(a2, a2) === 0);
    };
  }
  function ascendingDefined(a2, b) {
    return (a2 == null || !(a2 >= a2)) - (b == null || !(b >= b)) || (a2 < b ? -1 : a2 > b ? 1 : 0);
  }

  // node_modules/d3-array/src/array.js
  var array = Array.prototype;
  var slice = array.slice;
  var map = array.map;

  // node_modules/d3-array/src/constant.js
  function constant(x2) {
    return () => x2;
  }

  // node_modules/d3-array/src/ticks.js
  var e10 = Math.sqrt(50);
  var e5 = Math.sqrt(10);
  var e2 = Math.sqrt(2);
  function tickSpec(start2, stop, count3) {
    const step = (stop - start2) / Math.max(0, count3), power = Math.floor(Math.log10(step)), error = step / Math.pow(10, power), factor = error >= e10 ? 10 : error >= e5 ? 5 : error >= e2 ? 2 : 1;
    let i1, i2, inc;
    if (power < 0) {
      inc = Math.pow(10, -power) / factor;
      i1 = Math.round(start2 * inc);
      i2 = Math.round(stop * inc);
      if (i1 / inc < start2)
        ++i1;
      if (i2 / inc > stop)
        --i2;
      inc = -inc;
    } else {
      inc = Math.pow(10, power) * factor;
      i1 = Math.round(start2 / inc);
      i2 = Math.round(stop / inc);
      if (i1 * inc < start2)
        ++i1;
      if (i2 * inc > stop)
        --i2;
    }
    if (i2 < i1 && 0.5 <= count3 && count3 < 2)
      return tickSpec(start2, stop, count3 * 2);
    return [i1, i2, inc];
  }
  function ticks(start2, stop, count3) {
    stop = +stop, start2 = +start2, count3 = +count3;
    if (!(count3 > 0))
      return [];
    if (start2 === stop)
      return [start2];
    const reverse = stop < start2, [i1, i2, inc] = reverse ? tickSpec(stop, start2, count3) : tickSpec(start2, stop, count3);
    if (!(i2 >= i1))
      return [];
    const n = i2 - i1 + 1, ticks2 = new Array(n);
    if (reverse) {
      if (inc < 0)
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i2 - i) / -inc;
      else
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i2 - i) * inc;
    } else {
      if (inc < 0)
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i1 + i) / -inc;
      else
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i1 + i) * inc;
    }
    return ticks2;
  }
  function tickIncrement(start2, stop, count3) {
    stop = +stop, start2 = +start2, count3 = +count3;
    return tickSpec(start2, stop, count3)[2];
  }
  function tickStep(start2, stop, count3) {
    stop = +stop, start2 = +start2, count3 = +count3;
    const reverse = stop < start2, inc = reverse ? tickIncrement(stop, start2, count3) : tickIncrement(start2, stop, count3);
    return (reverse ? -1 : 1) * (inc < 0 ? 1 / -inc : inc);
  }

  // node_modules/d3-array/src/nice.js
  function nice(start2, stop, count3) {
    let prestep;
    while (true) {
      const step = tickIncrement(start2, stop, count3);
      if (step === prestep || step === 0 || !isFinite(step)) {
        return [start2, stop];
      } else if (step > 0) {
        start2 = Math.floor(start2 / step) * step;
        stop = Math.ceil(stop / step) * step;
      } else if (step < 0) {
        start2 = Math.ceil(start2 * step) / step;
        stop = Math.floor(stop * step) / step;
      }
      prestep = step;
    }
  }

  // node_modules/d3-array/src/threshold/sturges.js
  function thresholdSturges(values) {
    return Math.max(1, Math.ceil(Math.log(count(values)) / Math.LN2) + 1);
  }

  // node_modules/d3-array/src/bin.js
  function bin() {
    var value = identity, domain = extent, threshold = thresholdSturges;
    function histogram(data) {
      if (!Array.isArray(data))
        data = Array.from(data);
      var i, n = data.length, x2, step, values = new Array(n);
      for (i = 0; i < n; ++i) {
        values[i] = value(data[i], i, data);
      }
      var xz = domain(values), x0 = xz[0], x1 = xz[1], tz = threshold(values, x0, x1);
      if (!Array.isArray(tz)) {
        const max3 = x1, tn = +tz;
        if (domain === extent)
          [x0, x1] = nice(x0, x1, tn);
        tz = ticks(x0, x1, tn);
        if (tz[0] <= x0)
          step = tickIncrement(x0, x1, tn);
        if (tz[tz.length - 1] >= x1) {
          if (max3 >= x1 && domain === extent) {
            const step2 = tickIncrement(x0, x1, tn);
            if (isFinite(step2)) {
              if (step2 > 0) {
                x1 = (Math.floor(x1 / step2) + 1) * step2;
              } else if (step2 < 0) {
                x1 = (Math.ceil(x1 * -step2) + 1) / -step2;
              }
            }
          } else {
            tz.pop();
          }
        }
      }
      var m2 = tz.length, a2 = 0, b = m2;
      while (tz[a2] <= x0)
        ++a2;
      while (tz[b - 1] > x1)
        --b;
      if (a2 || b < m2)
        tz = tz.slice(a2, b), m2 = b - a2;
      var bins = new Array(m2 + 1), bin2;
      for (i = 0; i <= m2; ++i) {
        bin2 = bins[i] = [];
        bin2.x0 = i > 0 ? tz[i - 1] : x0;
        bin2.x1 = i < m2 ? tz[i] : x1;
      }
      if (isFinite(step)) {
        if (step > 0) {
          for (i = 0; i < n; ++i) {
            if ((x2 = values[i]) != null && x0 <= x2 && x2 <= x1) {
              bins[Math.min(m2, Math.floor((x2 - x0) / step))].push(data[i]);
            }
          }
        } else if (step < 0) {
          for (i = 0; i < n; ++i) {
            if ((x2 = values[i]) != null && x0 <= x2 && x2 <= x1) {
              const j = Math.floor((x0 - x2) * step);
              bins[Math.min(m2, j + (tz[j] <= x2))].push(data[i]);
            }
          }
        }
      } else {
        for (i = 0; i < n; ++i) {
          if ((x2 = values[i]) != null && x0 <= x2 && x2 <= x1) {
            bins[bisect_default(tz, x2, 0, m2)].push(data[i]);
          }
        }
      }
      return bins;
    }
    histogram.value = function(_) {
      return arguments.length ? (value = typeof _ === "function" ? _ : constant(_), histogram) : value;
    };
    histogram.domain = function(_) {
      return arguments.length ? (domain = typeof _ === "function" ? _ : constant([_[0], _[1]]), histogram) : domain;
    };
    histogram.thresholds = function(_) {
      return arguments.length ? (threshold = typeof _ === "function" ? _ : constant(Array.isArray(_) ? slice.call(_) : _), histogram) : threshold;
    };
    return histogram;
  }

  // node_modules/d3-array/src/max.js
  function max(values, valueof) {
    let max3;
    if (valueof === void 0) {
      for (const value of values) {
        if (value != null && (max3 < value || max3 === void 0 && value >= value)) {
          max3 = value;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (max3 < value || max3 === void 0 && value >= value)) {
          max3 = value;
        }
      }
    }
    return max3;
  }

  // node_modules/d3-array/src/min.js
  function min(values, valueof) {
    let min3;
    if (valueof === void 0) {
      for (const value of values) {
        if (value != null && (min3 > value || min3 === void 0 && value >= value)) {
          min3 = value;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (min3 > value || min3 === void 0 && value >= value)) {
          min3 = value;
        }
      }
    }
    return min3;
  }

  // node_modules/d3-array/src/quickselect.js
  function quickselect(array3, k, left2 = 0, right2 = Infinity, compare) {
    k = Math.floor(k);
    left2 = Math.floor(Math.max(0, left2));
    right2 = Math.floor(Math.min(array3.length - 1, right2));
    if (!(left2 <= k && k <= right2))
      return array3;
    compare = compare === void 0 ? ascendingDefined : compareDefined(compare);
    while (right2 > left2) {
      if (right2 - left2 > 600) {
        const n = right2 - left2 + 1;
        const m2 = k - left2 + 1;
        const z = Math.log(n);
        const s = 0.5 * Math.exp(2 * z / 3);
        const sd = 0.5 * Math.sqrt(z * s * (n - s) / n) * (m2 - n / 2 < 0 ? -1 : 1);
        const newLeft = Math.max(left2, Math.floor(k - m2 * s / n + sd));
        const newRight = Math.min(right2, Math.floor(k + (n - m2) * s / n + sd));
        quickselect(array3, k, newLeft, newRight, compare);
      }
      const t = array3[k];
      let i = left2;
      let j = right2;
      swap(array3, left2, k);
      if (compare(array3[right2], t) > 0)
        swap(array3, left2, right2);
      while (i < j) {
        swap(array3, i, j), ++i, --j;
        while (compare(array3[i], t) < 0)
          ++i;
        while (compare(array3[j], t) > 0)
          --j;
      }
      if (compare(array3[left2], t) === 0)
        swap(array3, left2, j);
      else
        ++j, swap(array3, j, right2);
      if (j <= k)
        left2 = j + 1;
      if (k <= j)
        right2 = j - 1;
    }
    return array3;
  }
  function swap(array3, i, j) {
    const t = array3[i];
    array3[i] = array3[j];
    array3[j] = t;
  }

  // node_modules/d3-array/src/quantile.js
  function quantile(values, p, valueof) {
    values = Float64Array.from(numbers(values, valueof));
    if (!(n = values.length) || isNaN(p = +p))
      return;
    if (p <= 0 || n < 2)
      return min(values);
    if (p >= 1)
      return max(values);
    var n, i = (n - 1) * p, i0 = Math.floor(i), value0 = max(quickselect(values, i0).subarray(0, i0 + 1)), value1 = min(values.subarray(i0 + 1));
    return value0 + (value1 - value0) * (i - i0);
  }

  // node_modules/d3-array/src/mean.js
  function mean(values, valueof) {
    let count3 = 0;
    let sum = 0;
    if (valueof === void 0) {
      for (let value of values) {
        if (value != null && (value = +value) >= value) {
          ++count3, sum += value;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (value = +value) >= value) {
          ++count3, sum += value;
        }
      }
    }
    if (count3)
      return sum / count3;
  }

  // node_modules/d3-array/src/range.js
  function range(start2, stop, step) {
    start2 = +start2, stop = +stop, step = (n = arguments.length) < 2 ? (stop = start2, start2 = 0, 1) : n < 3 ? 1 : +step;
    var i = -1, n = Math.max(0, Math.ceil((stop - start2) / step)) | 0, range2 = new Array(n);
    while (++i < n) {
      range2[i] = start2 + i * step;
    }
    return range2;
  }

  // node_modules/d3-axis/src/identity.js
  function identity_default(x2) {
    return x2;
  }

  // node_modules/d3-axis/src/axis.js
  var top = 1;
  var right = 2;
  var bottom = 3;
  var left = 4;
  var epsilon = 1e-6;
  function translateX(x2) {
    return "translate(" + x2 + ",0)";
  }
  function translateY(y2) {
    return "translate(0," + y2 + ")";
  }
  function number2(scale) {
    return (d) => +scale(d);
  }
  function center(scale, offset) {
    offset = Math.max(0, scale.bandwidth() - offset * 2) / 2;
    if (scale.round())
      offset = Math.round(offset);
    return (d) => +scale(d) + offset;
  }
  function entering() {
    return !this.__axis;
  }
  function axis(orient, scale) {
    var tickArguments = [], tickValues = null, tickFormat2 = null, tickSizeInner = 6, tickSizeOuter = 6, tickPadding = 3, offset = typeof window !== "undefined" && window.devicePixelRatio > 1 ? 0 : 0.5, k = orient === top || orient === left ? -1 : 1, x2 = orient === left || orient === right ? "x" : "y", transform2 = orient === top || orient === bottom ? translateX : translateY;
    function axis2(context) {
      var values = tickValues == null ? scale.ticks ? scale.ticks.apply(scale, tickArguments) : scale.domain() : tickValues, format2 = tickFormat2 == null ? scale.tickFormat ? scale.tickFormat.apply(scale, tickArguments) : identity_default : tickFormat2, spacing = Math.max(tickSizeInner, 0) + tickPadding, range2 = scale.range(), range0 = +range2[0] + offset, range1 = +range2[range2.length - 1] + offset, position = (scale.bandwidth ? center : number2)(scale.copy(), offset), selection2 = context.selection ? context.selection() : context, path2 = selection2.selectAll(".domain").data([null]), tick = selection2.selectAll(".tick").data(values, scale).order(), tickExit = tick.exit(), tickEnter = tick.enter().append("g").attr("class", "tick"), line = tick.select("line"), text = tick.select("text");
      path2 = path2.merge(path2.enter().insert("path", ".tick").attr("class", "domain").attr("stroke", "currentColor"));
      tick = tick.merge(tickEnter);
      line = line.merge(tickEnter.append("line").attr("stroke", "currentColor").attr(x2 + "2", k * tickSizeInner));
      text = text.merge(tickEnter.append("text").attr("fill", "currentColor").attr(x2, k * spacing).attr("dy", orient === top ? "0em" : orient === bottom ? "0.71em" : "0.32em"));
      if (context !== selection2) {
        path2 = path2.transition(context);
        tick = tick.transition(context);
        line = line.transition(context);
        text = text.transition(context);
        tickExit = tickExit.transition(context).attr("opacity", epsilon).attr("transform", function(d) {
          return isFinite(d = position(d)) ? transform2(d + offset) : this.getAttribute("transform");
        });
        tickEnter.attr("opacity", epsilon).attr("transform", function(d) {
          var p = this.parentNode.__axis;
          return transform2((p && isFinite(p = p(d)) ? p : position(d)) + offset);
        });
      }
      tickExit.remove();
      path2.attr("d", orient === left || orient === right ? tickSizeOuter ? "M" + k * tickSizeOuter + "," + range0 + "H" + offset + "V" + range1 + "H" + k * tickSizeOuter : "M" + offset + "," + range0 + "V" + range1 : tickSizeOuter ? "M" + range0 + "," + k * tickSizeOuter + "V" + offset + "H" + range1 + "V" + k * tickSizeOuter : "M" + range0 + "," + offset + "H" + range1);
      tick.attr("opacity", 1).attr("transform", function(d) {
        return transform2(position(d) + offset);
      });
      line.attr(x2 + "2", k * tickSizeInner);
      text.attr(x2, k * spacing).text(format2);
      selection2.filter(entering).attr("fill", "none").attr("font-size", 10).attr("font-family", "sans-serif").attr("text-anchor", orient === right ? "start" : orient === left ? "end" : "middle");
      selection2.each(function() {
        this.__axis = position;
      });
    }
    axis2.scale = function(_) {
      return arguments.length ? (scale = _, axis2) : scale;
    };
    axis2.ticks = function() {
      return tickArguments = Array.from(arguments), axis2;
    };
    axis2.tickArguments = function(_) {
      return arguments.length ? (tickArguments = _ == null ? [] : Array.from(_), axis2) : tickArguments.slice();
    };
    axis2.tickValues = function(_) {
      return arguments.length ? (tickValues = _ == null ? null : Array.from(_), axis2) : tickValues && tickValues.slice();
    };
    axis2.tickFormat = function(_) {
      return arguments.length ? (tickFormat2 = _, axis2) : tickFormat2;
    };
    axis2.tickSize = function(_) {
      return arguments.length ? (tickSizeInner = tickSizeOuter = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeInner = function(_) {
      return arguments.length ? (tickSizeInner = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeOuter = function(_) {
      return arguments.length ? (tickSizeOuter = +_, axis2) : tickSizeOuter;
    };
    axis2.tickPadding = function(_) {
      return arguments.length ? (tickPadding = +_, axis2) : tickPadding;
    };
    axis2.offset = function(_) {
      return arguments.length ? (offset = +_, axis2) : offset;
    };
    return axis2;
  }
  function axisRight(scale) {
    return axis(right, scale);
  }
  function axisBottom(scale) {
    return axis(bottom, scale);
  }
  function axisLeft(scale) {
    return axis(left, scale);
  }

  // node_modules/d3-dispatch/src/dispatch.js
  var noop = { value: () => {
  } };
  function dispatch() {
    for (var i = 0, n = arguments.length, _ = {}, t; i < n; ++i) {
      if (!(t = arguments[i] + "") || t in _ || /[\s.]/.test(t))
        throw new Error("illegal type: " + t);
      _[t] = [];
    }
    return new Dispatch(_);
  }
  function Dispatch(_) {
    this._ = _;
  }
  function parseTypenames(typenames, types) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name = "", i = t.indexOf(".");
      if (i >= 0)
        name = t.slice(i + 1), t = t.slice(0, i);
      if (t && !types.hasOwnProperty(t))
        throw new Error("unknown type: " + t);
      return { type: t, name };
    });
  }
  Dispatch.prototype = dispatch.prototype = {
    constructor: Dispatch,
    on: function(typename, callback) {
      var _ = this._, T = parseTypenames(typename + "", _), t, i = -1, n = T.length;
      if (arguments.length < 2) {
        while (++i < n)
          if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name)))
            return t;
        return;
      }
      if (callback != null && typeof callback !== "function")
        throw new Error("invalid callback: " + callback);
      while (++i < n) {
        if (t = (typename = T[i]).type)
          _[t] = set(_[t], typename.name, callback);
        else if (callback == null)
          for (t in _)
            _[t] = set(_[t], typename.name, null);
      }
      return this;
    },
    copy: function() {
      var copy3 = {}, _ = this._;
      for (var t in _)
        copy3[t] = _[t].slice();
      return new Dispatch(copy3);
    },
    call: function(type2, that) {
      if ((n = arguments.length - 2) > 0)
        for (var args = new Array(n), i = 0, n, t; i < n; ++i)
          args[i] = arguments[i + 2];
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    },
    apply: function(type2, that, args) {
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (var t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    }
  };
  function get(type2, name) {
    for (var i = 0, n = type2.length, c3; i < n; ++i) {
      if ((c3 = type2[i]).name === name) {
        return c3.value;
      }
    }
  }
  function set(type2, name, callback) {
    for (var i = 0, n = type2.length; i < n; ++i) {
      if (type2[i].name === name) {
        type2[i] = noop, type2 = type2.slice(0, i).concat(type2.slice(i + 1));
        break;
      }
    }
    if (callback != null)
      type2.push({ name, value: callback });
    return type2;
  }
  var dispatch_default = dispatch;

  // node_modules/d3-selection/src/namespaces.js
  var xhtml = "http://www.w3.org/1999/xhtml";
  var namespaces_default = {
    svg: "http://www.w3.org/2000/svg",
    xhtml,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-selection/src/namespace.js
  function namespace_default(name) {
    var prefix = name += "", i = prefix.indexOf(":");
    if (i >= 0 && (prefix = name.slice(0, i)) !== "xmlns")
      name = name.slice(i + 1);
    return namespaces_default.hasOwnProperty(prefix) ? { space: namespaces_default[prefix], local: name } : name;
  }

  // node_modules/d3-selection/src/creator.js
  function creatorInherit(name) {
    return function() {
      var document2 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml && document2.documentElement.namespaceURI === xhtml ? document2.createElement(name) : document2.createElementNS(uri, name);
    };
  }
  function creatorFixed(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default(name) {
    var fullname = namespace_default(name);
    return (fullname.local ? creatorFixed : creatorInherit)(fullname);
  }

  // node_modules/d3-selection/src/selector.js
  function none() {
  }
  function selector_default(selector) {
    return selector == null ? none : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-selection/src/selection/select.js
  function select_default(select) {
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m2 = groups.length, subgroups = new Array(m2), j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group[i]) && (subnode = select.call(node, node.__data__, i, group))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/array.js
  function array2(x2) {
    return x2 == null ? [] : Array.isArray(x2) ? x2 : Array.from(x2);
  }

  // node_modules/d3-selection/src/selectorAll.js
  function empty() {
    return [];
  }
  function selectorAll_default(selector) {
    return selector == null ? empty : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectAll.js
  function arrayAll(select) {
    return function() {
      return array2(select.apply(this, arguments));
    };
  }
  function selectAll_default(select) {
    if (typeof select === "function")
      select = arrayAll(select);
    else
      select = selectorAll_default(select);
    for (var groups = this._groups, m2 = groups.length, subgroups = [], parents = [], j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          subgroups.push(select.call(node, node.__data__, i, group));
          parents.push(node);
        }
      }
    }
    return new Selection(subgroups, parents);
  }

  // node_modules/d3-selection/src/matcher.js
  function matcher_default(selector) {
    return function() {
      return this.matches(selector);
    };
  }
  function childMatcher(selector) {
    return function(node) {
      return node.matches(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectChild.js
  var find = Array.prototype.find;
  function childFind(match) {
    return function() {
      return find.call(this.children, match);
    };
  }
  function childFirst() {
    return this.firstElementChild;
  }
  function selectChild_default(match) {
    return this.select(match == null ? childFirst : childFind(typeof match === "function" ? match : childMatcher(match)));
  }

  // node_modules/d3-selection/src/selection/selectChildren.js
  var filter = Array.prototype.filter;
  function children() {
    return Array.from(this.children);
  }
  function childrenFilter(match) {
    return function() {
      return filter.call(this.children, match);
    };
  }
  function selectChildren_default(match) {
    return this.selectAll(match == null ? children : childrenFilter(typeof match === "function" ? match : childMatcher(match)));
  }

  // node_modules/d3-selection/src/selection/filter.js
  function filter_default(match) {
    if (typeof match !== "function")
      match = matcher_default(match);
    for (var groups = this._groups, m2 = groups.length, subgroups = new Array(m2), j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group[i]) && match.call(node, node.__data__, i, group)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/selection/sparse.js
  function sparse_default(update) {
    return new Array(update.length);
  }

  // node_modules/d3-selection/src/selection/enter.js
  function enter_default() {
    return new Selection(this._enter || this._groups.map(sparse_default), this._parents);
  }
  function EnterNode(parent, datum2) {
    this.ownerDocument = parent.ownerDocument;
    this.namespaceURI = parent.namespaceURI;
    this._next = null;
    this._parent = parent;
    this.__data__ = datum2;
  }
  EnterNode.prototype = {
    constructor: EnterNode,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-selection/src/constant.js
  function constant_default(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-selection/src/selection/data.js
  function bindIndex(parent, group, enter, update, exit, data) {
    var i = 0, node, groupLength = group.length, dataLength = data.length;
    for (; i < dataLength; ++i) {
      if (node = group[i]) {
        node.__data__ = data[i];
        update[i] = node;
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (; i < groupLength; ++i) {
      if (node = group[i]) {
        exit[i] = node;
      }
    }
  }
  function bindKey(parent, group, enter, update, exit, data, key) {
    var i, node, nodeByKeyValue = /* @__PURE__ */ new Map(), groupLength = group.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i = 0; i < groupLength; ++i) {
      if (node = group[i]) {
        keyValues[i] = keyValue = key.call(node, node.__data__, i, group) + "";
        if (nodeByKeyValue.has(keyValue)) {
          exit[i] = node;
        } else {
          nodeByKeyValue.set(keyValue, node);
        }
      }
    }
    for (i = 0; i < dataLength; ++i) {
      keyValue = key.call(parent, data[i], i, data) + "";
      if (node = nodeByKeyValue.get(keyValue)) {
        update[i] = node;
        node.__data__ = data[i];
        nodeByKeyValue.delete(keyValue);
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (i = 0; i < groupLength; ++i) {
      if ((node = group[i]) && nodeByKeyValue.get(keyValues[i]) === node) {
        exit[i] = node;
      }
    }
  }
  function datum(node) {
    return node.__data__;
  }
  function data_default(value, key) {
    if (!arguments.length)
      return Array.from(this, datum);
    var bind = key ? bindKey : bindIndex, parents = this._parents, groups = this._groups;
    if (typeof value !== "function")
      value = constant_default(value);
    for (var m2 = groups.length, update = new Array(m2), enter = new Array(m2), exit = new Array(m2), j = 0; j < m2; ++j) {
      var parent = parents[j], group = groups[j], groupLength = group.length, data = arraylike(value.call(parent, parent && parent.__data__, j, parents)), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind(parent, group, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1)
            i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength)
            ;
          previous._next = next || null;
        }
      }
    }
    update = new Selection(update, parents);
    update._enter = enter;
    update._exit = exit;
    return update;
  }
  function arraylike(data) {
    return typeof data === "object" && "length" in data ? data : Array.from(data);
  }

  // node_modules/d3-selection/src/selection/exit.js
  function exit_default() {
    return new Selection(this._exit || this._groups.map(sparse_default), this._parents);
  }

  // node_modules/d3-selection/src/selection/join.js
  function join_default(onenter, onupdate, onexit) {
    var enter = this.enter(), update = this, exit = this.exit();
    if (typeof onenter === "function") {
      enter = onenter(enter);
      if (enter)
        enter = enter.selection();
    } else {
      enter = enter.append(onenter + "");
    }
    if (onupdate != null) {
      update = onupdate(update);
      if (update)
        update = update.selection();
    }
    if (onexit == null)
      exit.remove();
    else
      onexit(exit);
    return enter && update ? enter.merge(update).order() : update;
  }

  // node_modules/d3-selection/src/selection/merge.js
  function merge_default(context) {
    var selection2 = context.selection ? context.selection() : context;
    for (var groups0 = this._groups, groups1 = selection2._groups, m0 = groups0.length, m1 = groups1.length, m2 = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m2; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection(merges, this._parents);
  }

  // node_modules/d3-selection/src/selection/order.js
  function order_default() {
    for (var groups = this._groups, j = -1, m2 = groups.length; ++j < m2; ) {
      for (var group = groups[j], i = group.length - 1, next = group[i], node; --i >= 0; ) {
        if (node = group[i]) {
          if (next && node.compareDocumentPosition(next) ^ 4)
            next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/sort.js
  function sort_default(compare) {
    if (!compare)
      compare = ascending2;
    function compareNode(a2, b) {
      return a2 && b ? compare(a2.__data__, b.__data__) : !a2 - !b;
    }
    for (var groups = this._groups, m2 = groups.length, sortgroups = new Array(m2), j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          sortgroup[i] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection(sortgroups, this._parents).order();
  }
  function ascending2(a2, b) {
    return a2 < b ? -1 : a2 > b ? 1 : a2 >= b ? 0 : NaN;
  }

  // node_modules/d3-selection/src/selection/call.js
  function call_default() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-selection/src/selection/nodes.js
  function nodes_default() {
    return Array.from(this);
  }

  // node_modules/d3-selection/src/selection/node.js
  function node_default() {
    for (var groups = this._groups, j = 0, m2 = groups.length; j < m2; ++j) {
      for (var group = groups[j], i = 0, n = group.length; i < n; ++i) {
        var node = group[i];
        if (node)
          return node;
      }
    }
    return null;
  }

  // node_modules/d3-selection/src/selection/size.js
  function size_default() {
    let size = 0;
    for (const node of this)
      ++size;
    return size;
  }

  // node_modules/d3-selection/src/selection/empty.js
  function empty_default() {
    return !this.node();
  }

  // node_modules/d3-selection/src/selection/each.js
  function each_default(callback) {
    for (var groups = this._groups, j = 0, m2 = groups.length; j < m2; ++j) {
      for (var group = groups[j], i = 0, n = group.length, node; i < n; ++i) {
        if (node = group[i])
          callback.call(node, node.__data__, i, group);
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/attr.js
  function attrRemove(name) {
    return function() {
      this.removeAttribute(name);
    };
  }
  function attrRemoveNS(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant(name, value) {
    return function() {
      this.setAttribute(name, value);
    };
  }
  function attrConstantNS(fullname, value) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value);
    };
  }
  function attrFunction(name, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttribute(name);
      else
        this.setAttribute(name, v);
    };
  }
  function attrFunctionNS(fullname, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttributeNS(fullname.space, fullname.local);
      else
        this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default(name, value) {
    var fullname = namespace_default(name);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value == null ? fullname.local ? attrRemoveNS : attrRemove : typeof value === "function" ? fullname.local ? attrFunctionNS : attrFunction : fullname.local ? attrConstantNS : attrConstant)(fullname, value));
  }

  // node_modules/d3-selection/src/window.js
  function window_default(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-selection/src/selection/style.js
  function styleRemove(name) {
    return function() {
      this.style.removeProperty(name);
    };
  }
  function styleConstant(name, value, priority) {
    return function() {
      this.style.setProperty(name, value, priority);
    };
  }
  function styleFunction(name, value, priority) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.style.removeProperty(name);
      else
        this.style.setProperty(name, v, priority);
    };
  }
  function style_default(name, value, priority) {
    return arguments.length > 1 ? this.each((value == null ? styleRemove : typeof value === "function" ? styleFunction : styleConstant)(name, value, priority == null ? "" : priority)) : styleValue(this.node(), name);
  }
  function styleValue(node, name) {
    return node.style.getPropertyValue(name) || window_default(node).getComputedStyle(node, null).getPropertyValue(name);
  }

  // node_modules/d3-selection/src/selection/property.js
  function propertyRemove(name) {
    return function() {
      delete this[name];
    };
  }
  function propertyConstant(name, value) {
    return function() {
      this[name] = value;
    };
  }
  function propertyFunction(name, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        delete this[name];
      else
        this[name] = v;
    };
  }
  function property_default(name, value) {
    return arguments.length > 1 ? this.each((value == null ? propertyRemove : typeof value === "function" ? propertyFunction : propertyConstant)(name, value)) : this.node()[name];
  }

  // node_modules/d3-selection/src/selection/classed.js
  function classArray(string) {
    return string.trim().split(/^|\s+/);
  }
  function classList(node) {
    return node.classList || new ClassList(node);
  }
  function ClassList(node) {
    this._node = node;
    this._names = classArray(node.getAttribute("class") || "");
  }
  ClassList.prototype = {
    add: function(name) {
      var i = this._names.indexOf(name);
      if (i < 0) {
        this._names.push(name);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name) {
      var i = this._names.indexOf(name);
      if (i >= 0) {
        this._names.splice(i, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name) {
      return this._names.indexOf(name) >= 0;
    }
  };
  function classedAdd(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.add(names[i]);
  }
  function classedRemove(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.remove(names[i]);
  }
  function classedTrue(names) {
    return function() {
      classedAdd(this, names);
    };
  }
  function classedFalse(names) {
    return function() {
      classedRemove(this, names);
    };
  }
  function classedFunction(names, value) {
    return function() {
      (value.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
    };
  }
  function classed_default(name, value) {
    var names = classArray(name + "");
    if (arguments.length < 2) {
      var list = classList(this.node()), i = -1, n = names.length;
      while (++i < n)
        if (!list.contains(names[i]))
          return false;
      return true;
    }
    return this.each((typeof value === "function" ? classedFunction : value ? classedTrue : classedFalse)(names, value));
  }

  // node_modules/d3-selection/src/selection/text.js
  function textRemove() {
    this.textContent = "";
  }
  function textConstant(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default(value) {
    return arguments.length ? this.each(value == null ? textRemove : (typeof value === "function" ? textFunction : textConstant)(value)) : this.node().textContent;
  }

  // node_modules/d3-selection/src/selection/html.js
  function htmlRemove() {
    this.innerHTML = "";
  }
  function htmlConstant(value) {
    return function() {
      this.innerHTML = value;
    };
  }
  function htmlFunction(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default(value) {
    return arguments.length ? this.each(value == null ? htmlRemove : (typeof value === "function" ? htmlFunction : htmlConstant)(value)) : this.node().innerHTML;
  }

  // node_modules/d3-selection/src/selection/raise.js
  function raise() {
    if (this.nextSibling)
      this.parentNode.appendChild(this);
  }
  function raise_default() {
    return this.each(raise);
  }

  // node_modules/d3-selection/src/selection/lower.js
  function lower() {
    if (this.previousSibling)
      this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default() {
    return this.each(lower);
  }

  // node_modules/d3-selection/src/selection/append.js
  function append_default(name) {
    var create2 = typeof name === "function" ? name : creator_default(name);
    return this.select(function() {
      return this.appendChild(create2.apply(this, arguments));
    });
  }

  // node_modules/d3-selection/src/selection/insert.js
  function constantNull() {
    return null;
  }
  function insert_default(name, before) {
    var create2 = typeof name === "function" ? name : creator_default(name), select = before == null ? constantNull : typeof before === "function" ? before : selector_default(before);
    return this.select(function() {
      return this.insertBefore(create2.apply(this, arguments), select.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-selection/src/selection/remove.js
  function remove() {
    var parent = this.parentNode;
    if (parent)
      parent.removeChild(this);
  }
  function remove_default() {
    return this.each(remove);
  }

  // node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow() {
    var clone = this.cloneNode(false), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function selection_cloneDeep() {
    var clone = this.cloneNode(true), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function clone_default(deep) {
    return this.select(deep ? selection_cloneDeep : selection_cloneShallow);
  }

  // node_modules/d3-selection/src/selection/datum.js
  function datum_default(value) {
    return arguments.length ? this.property("__data__", value) : this.node().__data__;
  }

  // node_modules/d3-selection/src/selection/on.js
  function contextListener(listener) {
    return function(event) {
      listener.call(this, event, this.__data__);
    };
  }
  function parseTypenames2(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name = "", i = t.indexOf(".");
      if (i >= 0)
        name = t.slice(i + 1), t = t.slice(0, i);
      return { type: t, name };
    });
  }
  function onRemove(typename) {
    return function() {
      var on = this.__on;
      if (!on)
        return;
      for (var j = 0, i = -1, m2 = on.length, o; j < m2; ++j) {
        if (o = on[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
        } else {
          on[++i] = o;
        }
      }
      if (++i)
        on.length = i;
      else
        delete this.__on;
    };
  }
  function onAdd(typename, value, options) {
    return function() {
      var on = this.__on, o, listener = contextListener(value);
      if (on)
        for (var j = 0, m2 = on.length; j < m2; ++j) {
          if ((o = on[j]).type === typename.type && o.name === typename.name) {
            this.removeEventListener(o.type, o.listener, o.options);
            this.addEventListener(o.type, o.listener = listener, o.options = options);
            o.value = value;
            return;
          }
        }
      this.addEventListener(typename.type, listener, options);
      o = { type: typename.type, name: typename.name, value, listener, options };
      if (!on)
        this.__on = [o];
      else
        on.push(o);
    };
  }
  function on_default(typename, value, options) {
    var typenames = parseTypenames2(typename + ""), i, n = typenames.length, t;
    if (arguments.length < 2) {
      var on = this.node().__on;
      if (on)
        for (var j = 0, m2 = on.length, o; j < m2; ++j) {
          for (i = 0, o = on[j]; i < n; ++i) {
            if ((t = typenames[i]).type === o.type && t.name === o.name) {
              return o.value;
            }
          }
        }
      return;
    }
    on = value ? onAdd : onRemove;
    for (i = 0; i < n; ++i)
      this.each(on(typenames[i], value, options));
    return this;
  }

  // node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent(node, type2, params) {
    var window2 = window_default(node), event = window2.CustomEvent;
    if (typeof event === "function") {
      event = new event(type2, params);
    } else {
      event = window2.document.createEvent("Event");
      if (params)
        event.initEvent(type2, params.bubbles, params.cancelable), event.detail = params.detail;
      else
        event.initEvent(type2, false, false);
    }
    node.dispatchEvent(event);
  }
  function dispatchConstant(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params);
    };
  }
  function dispatchFunction(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default2(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction : dispatchConstant)(type2, params));
  }

  // node_modules/d3-selection/src/selection/iterator.js
  function* iterator_default() {
    for (var groups = this._groups, j = 0, m2 = groups.length; j < m2; ++j) {
      for (var group = groups[j], i = 0, n = group.length, node; i < n; ++i) {
        if (node = group[i])
          yield node;
      }
    }
  }

  // node_modules/d3-selection/src/selection/index.js
  var root = [null];
  function Selection(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection() {
    return new Selection([[document.documentElement]], root);
  }
  function selection_selection() {
    return this;
  }
  Selection.prototype = selection.prototype = {
    constructor: Selection,
    select: select_default,
    selectAll: selectAll_default,
    selectChild: selectChild_default,
    selectChildren: selectChildren_default,
    filter: filter_default,
    data: data_default,
    enter: enter_default,
    exit: exit_default,
    join: join_default,
    merge: merge_default,
    selection: selection_selection,
    order: order_default,
    sort: sort_default,
    call: call_default,
    nodes: nodes_default,
    node: node_default,
    size: size_default,
    empty: empty_default,
    each: each_default,
    attr: attr_default,
    style: style_default,
    property: property_default,
    classed: classed_default,
    text: text_default,
    html: html_default,
    raise: raise_default,
    lower: lower_default,
    append: append_default,
    insert: insert_default,
    remove: remove_default,
    clone: clone_default,
    datum: datum_default,
    on: on_default,
    dispatch: dispatch_default2,
    [Symbol.iterator]: iterator_default
  };
  var selection_default = selection;

  // node_modules/d3-selection/src/select.js
  function select_default2(selector) {
    return typeof selector === "string" ? new Selection([[document.querySelector(selector)]], [document.documentElement]) : new Selection([[selector]], root);
  }

  // node_modules/d3-selection/src/sourceEvent.js
  function sourceEvent_default(event) {
    let sourceEvent;
    while (sourceEvent = event.sourceEvent)
      event = sourceEvent;
    return event;
  }

  // node_modules/d3-selection/src/pointer.js
  function pointer_default(event, node) {
    event = sourceEvent_default(event);
    if (node === void 0)
      node = event.currentTarget;
    if (node) {
      var svg = node.ownerSVGElement || node;
      if (svg.createSVGPoint) {
        var point3 = svg.createSVGPoint();
        point3.x = event.clientX, point3.y = event.clientY;
        point3 = point3.matrixTransform(node.getScreenCTM().inverse());
        return [point3.x, point3.y];
      }
      if (node.getBoundingClientRect) {
        var rect = node.getBoundingClientRect();
        return [event.clientX - rect.left - node.clientLeft, event.clientY - rect.top - node.clientTop];
      }
    }
    return [event.pageX, event.pageY];
  }

  // node_modules/d3-selection/src/selectAll.js
  function selectAll_default2(selector) {
    return typeof selector === "string" ? new Selection([document.querySelectorAll(selector)], [document.documentElement]) : new Selection([array2(selector)], root);
  }

  // node_modules/d3-drag/src/noevent.js
  var nonpassivecapture = { capture: true, passive: false };
  function noevent_default(event) {
    event.preventDefault();
    event.stopImmediatePropagation();
  }

  // node_modules/d3-drag/src/nodrag.js
  function nodrag_default(view) {
    var root2 = view.document.documentElement, selection2 = select_default2(view).on("dragstart.drag", noevent_default, nonpassivecapture);
    if ("onselectstart" in root2) {
      selection2.on("selectstart.drag", noevent_default, nonpassivecapture);
    } else {
      root2.__noselect = root2.style.MozUserSelect;
      root2.style.MozUserSelect = "none";
    }
  }
  function yesdrag(view, noclick) {
    var root2 = view.document.documentElement, selection2 = select_default2(view).on("dragstart.drag", null);
    if (noclick) {
      selection2.on("click.drag", noevent_default, nonpassivecapture);
      setTimeout(function() {
        selection2.on("click.drag", null);
      }, 0);
    }
    if ("onselectstart" in root2) {
      selection2.on("selectstart.drag", null);
    } else {
      root2.style.MozUserSelect = root2.__noselect;
      delete root2.__noselect;
    }
  }

  // node_modules/d3-color/src/define.js
  function define_default(constructor, factory, prototype) {
    constructor.prototype = factory.prototype = prototype;
    prototype.constructor = constructor;
  }
  function extend(parent, definition) {
    var prototype = Object.create(parent.prototype);
    for (var key in definition)
      prototype[key] = definition[key];
    return prototype;
  }

  // node_modules/d3-color/src/color.js
  function Color() {
  }
  var darker = 0.7;
  var brighter = 1 / darker;
  var reI = "\\s*([+-]?\\d+)\\s*";
  var reN = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)\\s*";
  var reP = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)%\\s*";
  var reHex = /^#([0-9a-f]{3,8})$/;
  var reRgbInteger = new RegExp(`^rgb\\(${reI},${reI},${reI}\\)$`);
  var reRgbPercent = new RegExp(`^rgb\\(${reP},${reP},${reP}\\)$`);
  var reRgbaInteger = new RegExp(`^rgba\\(${reI},${reI},${reI},${reN}\\)$`);
  var reRgbaPercent = new RegExp(`^rgba\\(${reP},${reP},${reP},${reN}\\)$`);
  var reHslPercent = new RegExp(`^hsl\\(${reN},${reP},${reP}\\)$`);
  var reHslaPercent = new RegExp(`^hsla\\(${reN},${reP},${reP},${reN}\\)$`);
  var named = {
    aliceblue: 15792383,
    antiquewhite: 16444375,
    aqua: 65535,
    aquamarine: 8388564,
    azure: 15794175,
    beige: 16119260,
    bisque: 16770244,
    black: 0,
    blanchedalmond: 16772045,
    blue: 255,
    blueviolet: 9055202,
    brown: 10824234,
    burlywood: 14596231,
    cadetblue: 6266528,
    chartreuse: 8388352,
    chocolate: 13789470,
    coral: 16744272,
    cornflowerblue: 6591981,
    cornsilk: 16775388,
    crimson: 14423100,
    cyan: 65535,
    darkblue: 139,
    darkcyan: 35723,
    darkgoldenrod: 12092939,
    darkgray: 11119017,
    darkgreen: 25600,
    darkgrey: 11119017,
    darkkhaki: 12433259,
    darkmagenta: 9109643,
    darkolivegreen: 5597999,
    darkorange: 16747520,
    darkorchid: 10040012,
    darkred: 9109504,
    darksalmon: 15308410,
    darkseagreen: 9419919,
    darkslateblue: 4734347,
    darkslategray: 3100495,
    darkslategrey: 3100495,
    darkturquoise: 52945,
    darkviolet: 9699539,
    deeppink: 16716947,
    deepskyblue: 49151,
    dimgray: 6908265,
    dimgrey: 6908265,
    dodgerblue: 2003199,
    firebrick: 11674146,
    floralwhite: 16775920,
    forestgreen: 2263842,
    fuchsia: 16711935,
    gainsboro: 14474460,
    ghostwhite: 16316671,
    gold: 16766720,
    goldenrod: 14329120,
    gray: 8421504,
    green: 32768,
    greenyellow: 11403055,
    grey: 8421504,
    honeydew: 15794160,
    hotpink: 16738740,
    indianred: 13458524,
    indigo: 4915330,
    ivory: 16777200,
    khaki: 15787660,
    lavender: 15132410,
    lavenderblush: 16773365,
    lawngreen: 8190976,
    lemonchiffon: 16775885,
    lightblue: 11393254,
    lightcoral: 15761536,
    lightcyan: 14745599,
    lightgoldenrodyellow: 16448210,
    lightgray: 13882323,
    lightgreen: 9498256,
    lightgrey: 13882323,
    lightpink: 16758465,
    lightsalmon: 16752762,
    lightseagreen: 2142890,
    lightskyblue: 8900346,
    lightslategray: 7833753,
    lightslategrey: 7833753,
    lightsteelblue: 11584734,
    lightyellow: 16777184,
    lime: 65280,
    limegreen: 3329330,
    linen: 16445670,
    magenta: 16711935,
    maroon: 8388608,
    mediumaquamarine: 6737322,
    mediumblue: 205,
    mediumorchid: 12211667,
    mediumpurple: 9662683,
    mediumseagreen: 3978097,
    mediumslateblue: 8087790,
    mediumspringgreen: 64154,
    mediumturquoise: 4772300,
    mediumvioletred: 13047173,
    midnightblue: 1644912,
    mintcream: 16121850,
    mistyrose: 16770273,
    moccasin: 16770229,
    navajowhite: 16768685,
    navy: 128,
    oldlace: 16643558,
    olive: 8421376,
    olivedrab: 7048739,
    orange: 16753920,
    orangered: 16729344,
    orchid: 14315734,
    palegoldenrod: 15657130,
    palegreen: 10025880,
    paleturquoise: 11529966,
    palevioletred: 14381203,
    papayawhip: 16773077,
    peachpuff: 16767673,
    peru: 13468991,
    pink: 16761035,
    plum: 14524637,
    powderblue: 11591910,
    purple: 8388736,
    rebeccapurple: 6697881,
    red: 16711680,
    rosybrown: 12357519,
    royalblue: 4286945,
    saddlebrown: 9127187,
    salmon: 16416882,
    sandybrown: 16032864,
    seagreen: 3050327,
    seashell: 16774638,
    sienna: 10506797,
    silver: 12632256,
    skyblue: 8900331,
    slateblue: 6970061,
    slategray: 7372944,
    slategrey: 7372944,
    snow: 16775930,
    springgreen: 65407,
    steelblue: 4620980,
    tan: 13808780,
    teal: 32896,
    thistle: 14204888,
    tomato: 16737095,
    turquoise: 4251856,
    violet: 15631086,
    wheat: 16113331,
    white: 16777215,
    whitesmoke: 16119285,
    yellow: 16776960,
    yellowgreen: 10145074
  };
  define_default(Color, color, {
    copy(channels) {
      return Object.assign(new this.constructor(), this, channels);
    },
    displayable() {
      return this.rgb().displayable();
    },
    hex: color_formatHex,
    // Deprecated! Use color.formatHex.
    formatHex: color_formatHex,
    formatHex8: color_formatHex8,
    formatHsl: color_formatHsl,
    formatRgb: color_formatRgb,
    toString: color_formatRgb
  });
  function color_formatHex() {
    return this.rgb().formatHex();
  }
  function color_formatHex8() {
    return this.rgb().formatHex8();
  }
  function color_formatHsl() {
    return hslConvert(this).formatHsl();
  }
  function color_formatRgb() {
    return this.rgb().formatRgb();
  }
  function color(format2) {
    var m2, l;
    format2 = (format2 + "").trim().toLowerCase();
    return (m2 = reHex.exec(format2)) ? (l = m2[1].length, m2 = parseInt(m2[1], 16), l === 6 ? rgbn(m2) : l === 3 ? new Rgb(m2 >> 8 & 15 | m2 >> 4 & 240, m2 >> 4 & 15 | m2 & 240, (m2 & 15) << 4 | m2 & 15, 1) : l === 8 ? rgba(m2 >> 24 & 255, m2 >> 16 & 255, m2 >> 8 & 255, (m2 & 255) / 255) : l === 4 ? rgba(m2 >> 12 & 15 | m2 >> 8 & 240, m2 >> 8 & 15 | m2 >> 4 & 240, m2 >> 4 & 15 | m2 & 240, ((m2 & 15) << 4 | m2 & 15) / 255) : null) : (m2 = reRgbInteger.exec(format2)) ? new Rgb(m2[1], m2[2], m2[3], 1) : (m2 = reRgbPercent.exec(format2)) ? new Rgb(m2[1] * 255 / 100, m2[2] * 255 / 100, m2[3] * 255 / 100, 1) : (m2 = reRgbaInteger.exec(format2)) ? rgba(m2[1], m2[2], m2[3], m2[4]) : (m2 = reRgbaPercent.exec(format2)) ? rgba(m2[1] * 255 / 100, m2[2] * 255 / 100, m2[3] * 255 / 100, m2[4]) : (m2 = reHslPercent.exec(format2)) ? hsla(m2[1], m2[2] / 100, m2[3] / 100, 1) : (m2 = reHslaPercent.exec(format2)) ? hsla(m2[1], m2[2] / 100, m2[3] / 100, m2[4]) : named.hasOwnProperty(format2) ? rgbn(named[format2]) : format2 === "transparent" ? new Rgb(NaN, NaN, NaN, 0) : null;
  }
  function rgbn(n) {
    return new Rgb(n >> 16 & 255, n >> 8 & 255, n & 255, 1);
  }
  function rgba(r, g, b, a2) {
    if (a2 <= 0)
      r = g = b = NaN;
    return new Rgb(r, g, b, a2);
  }
  function rgbConvert(o) {
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Rgb();
    o = o.rgb();
    return new Rgb(o.r, o.g, o.b, o.opacity);
  }
  function rgb(r, g, b, opacity) {
    return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b, opacity == null ? 1 : opacity);
  }
  function Rgb(r, g, b, opacity) {
    this.r = +r;
    this.g = +g;
    this.b = +b;
    this.opacity = +opacity;
  }
  define_default(Rgb, rgb, extend(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    rgb() {
      return this;
    },
    clamp() {
      return new Rgb(clampi(this.r), clampi(this.g), clampi(this.b), clampa(this.opacity));
    },
    displayable() {
      return -0.5 <= this.r && this.r < 255.5 && (-0.5 <= this.g && this.g < 255.5) && (-0.5 <= this.b && this.b < 255.5) && (0 <= this.opacity && this.opacity <= 1);
    },
    hex: rgb_formatHex,
    // Deprecated! Use color.formatHex.
    formatHex: rgb_formatHex,
    formatHex8: rgb_formatHex8,
    formatRgb: rgb_formatRgb,
    toString: rgb_formatRgb
  }));
  function rgb_formatHex() {
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}`;
  }
  function rgb_formatHex8() {
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}${hex((isNaN(this.opacity) ? 1 : this.opacity) * 255)}`;
  }
  function rgb_formatRgb() {
    const a2 = clampa(this.opacity);
    return `${a2 === 1 ? "rgb(" : "rgba("}${clampi(this.r)}, ${clampi(this.g)}, ${clampi(this.b)}${a2 === 1 ? ")" : `, ${a2})`}`;
  }
  function clampa(opacity) {
    return isNaN(opacity) ? 1 : Math.max(0, Math.min(1, opacity));
  }
  function clampi(value) {
    return Math.max(0, Math.min(255, Math.round(value) || 0));
  }
  function hex(value) {
    value = clampi(value);
    return (value < 16 ? "0" : "") + value.toString(16);
  }
  function hsla(h, s, l, a2) {
    if (a2 <= 0)
      h = s = l = NaN;
    else if (l <= 0 || l >= 1)
      h = s = NaN;
    else if (s <= 0)
      h = NaN;
    return new Hsl(h, s, l, a2);
  }
  function hslConvert(o) {
    if (o instanceof Hsl)
      return new Hsl(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Hsl();
    if (o instanceof Hsl)
      return o;
    o = o.rgb();
    var r = o.r / 255, g = o.g / 255, b = o.b / 255, min3 = Math.min(r, g, b), max3 = Math.max(r, g, b), h = NaN, s = max3 - min3, l = (max3 + min3) / 2;
    if (s) {
      if (r === max3)
        h = (g - b) / s + (g < b) * 6;
      else if (g === max3)
        h = (b - r) / s + 2;
      else
        h = (r - g) / s + 4;
      s /= l < 0.5 ? max3 + min3 : 2 - max3 - min3;
      h *= 60;
    } else {
      s = l > 0 && l < 1 ? 0 : h;
    }
    return new Hsl(h, s, l, o.opacity);
  }
  function hsl(h, s, l, opacity) {
    return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
  }
  function Hsl(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Hsl, hsl, extend(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    rgb() {
      var h = this.h % 360 + (this.h < 0) * 360, s = isNaN(h) || isNaN(this.s) ? 0 : this.s, l = this.l, m2 = l + (l < 0.5 ? l : 1 - l) * s, m1 = 2 * l - m2;
      return new Rgb(
        hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
        hsl2rgb(h, m1, m2),
        hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
        this.opacity
      );
    },
    clamp() {
      return new Hsl(clamph(this.h), clampt(this.s), clampt(this.l), clampa(this.opacity));
    },
    displayable() {
      return (0 <= this.s && this.s <= 1 || isNaN(this.s)) && (0 <= this.l && this.l <= 1) && (0 <= this.opacity && this.opacity <= 1);
    },
    formatHsl() {
      const a2 = clampa(this.opacity);
      return `${a2 === 1 ? "hsl(" : "hsla("}${clamph(this.h)}, ${clampt(this.s) * 100}%, ${clampt(this.l) * 100}%${a2 === 1 ? ")" : `, ${a2})`}`;
    }
  }));
  function clamph(value) {
    value = (value || 0) % 360;
    return value < 0 ? value + 360 : value;
  }
  function clampt(value) {
    return Math.max(0, Math.min(1, value || 0));
  }
  function hsl2rgb(h, m1, m2) {
    return (h < 60 ? m1 + (m2 - m1) * h / 60 : h < 180 ? m2 : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60 : m1) * 255;
  }

  // node_modules/d3-color/src/math.js
  var radians = Math.PI / 180;
  var degrees = 180 / Math.PI;

  // node_modules/d3-color/src/cubehelix.js
  var A = -0.14861;
  var B = 1.78277;
  var C = -0.29227;
  var D = -0.90649;
  var E = 1.97294;
  var ED = E * D;
  var EB = E * B;
  var BC_DA = B * C - D * A;
  function cubehelixConvert(o) {
    if (o instanceof Cubehelix)
      return new Cubehelix(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Rgb))
      o = rgbConvert(o);
    var r = o.r / 255, g = o.g / 255, b = o.b / 255, l = (BC_DA * b + ED * r - EB * g) / (BC_DA + ED - EB), bl = b - l, k = (E * (g - l) - C * bl) / D, s = Math.sqrt(k * k + bl * bl) / (E * l * (1 - l)), h = s ? Math.atan2(k, bl) * degrees - 120 : NaN;
    return new Cubehelix(h < 0 ? h + 360 : h, s, l, o.opacity);
  }
  function cubehelix(h, s, l, opacity) {
    return arguments.length === 1 ? cubehelixConvert(h) : new Cubehelix(h, s, l, opacity == null ? 1 : opacity);
  }
  function Cubehelix(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Cubehelix, cubehelix, extend(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Cubehelix(this.h, this.s, this.l * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Cubehelix(this.h, this.s, this.l * k, this.opacity);
    },
    rgb() {
      var h = isNaN(this.h) ? 0 : (this.h + 120) * radians, l = +this.l, a2 = isNaN(this.s) ? 0 : this.s * l * (1 - l), cosh = Math.cos(h), sinh = Math.sin(h);
      return new Rgb(
        255 * (l + a2 * (A * cosh + B * sinh)),
        255 * (l + a2 * (C * cosh + D * sinh)),
        255 * (l + a2 * (E * cosh)),
        this.opacity
      );
    }
  }));

  // node_modules/d3-interpolate/src/basis.js
  function basis(t1, v0, v1, v2, v3) {
    var t2 = t1 * t1, t3 = t2 * t1;
    return ((1 - 3 * t1 + 3 * t2 - t3) * v0 + (4 - 6 * t2 + 3 * t3) * v1 + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2 + t3 * v3) / 6;
  }
  function basis_default(values) {
    var n = values.length - 1;
    return function(t) {
      var i = t <= 0 ? t = 0 : t >= 1 ? (t = 1, n - 1) : Math.floor(t * n), v1 = values[i], v2 = values[i + 1], v0 = i > 0 ? values[i - 1] : 2 * v1 - v2, v3 = i < n - 1 ? values[i + 2] : 2 * v2 - v1;
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/basisClosed.js
  function basisClosed_default(values) {
    var n = values.length;
    return function(t) {
      var i = Math.floor(((t %= 1) < 0 ? ++t : t) * n), v0 = values[(i + n - 1) % n], v1 = values[i % n], v2 = values[(i + 1) % n], v3 = values[(i + 2) % n];
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/constant.js
  var constant_default2 = (x2) => () => x2;

  // node_modules/d3-interpolate/src/color.js
  function linear(a2, d) {
    return function(t) {
      return a2 + t * d;
    };
  }
  function exponential(a2, b, y2) {
    return a2 = Math.pow(a2, y2), b = Math.pow(b, y2) - a2, y2 = 1 / y2, function(t) {
      return Math.pow(a2 + t * b, y2);
    };
  }
  function hue(a2, b) {
    var d = b - a2;
    return d ? linear(a2, d > 180 || d < -180 ? d - 360 * Math.round(d / 360) : d) : constant_default2(isNaN(a2) ? b : a2);
  }
  function gamma(y2) {
    return (y2 = +y2) === 1 ? nogamma : function(a2, b) {
      return b - a2 ? exponential(a2, b, y2) : constant_default2(isNaN(a2) ? b : a2);
    };
  }
  function nogamma(a2, b) {
    var d = b - a2;
    return d ? linear(a2, d) : constant_default2(isNaN(a2) ? b : a2);
  }

  // node_modules/d3-interpolate/src/rgb.js
  var rgb_default = function rgbGamma(y2) {
    var color2 = gamma(y2);
    function rgb2(start2, end) {
      var r = color2((start2 = rgb(start2)).r, (end = rgb(end)).r), g = color2(start2.g, end.g), b = color2(start2.b, end.b), opacity = nogamma(start2.opacity, end.opacity);
      return function(t) {
        start2.r = r(t);
        start2.g = g(t);
        start2.b = b(t);
        start2.opacity = opacity(t);
        return start2 + "";
      };
    }
    rgb2.gamma = rgbGamma;
    return rgb2;
  }(1);
  function rgbSpline(spline) {
    return function(colors) {
      var n = colors.length, r = new Array(n), g = new Array(n), b = new Array(n), i, color2;
      for (i = 0; i < n; ++i) {
        color2 = rgb(colors[i]);
        r[i] = color2.r || 0;
        g[i] = color2.g || 0;
        b[i] = color2.b || 0;
      }
      r = spline(r);
      g = spline(g);
      b = spline(b);
      color2.opacity = 1;
      return function(t) {
        color2.r = r(t);
        color2.g = g(t);
        color2.b = b(t);
        return color2 + "";
      };
    };
  }
  var rgbBasis = rgbSpline(basis_default);
  var rgbBasisClosed = rgbSpline(basisClosed_default);

  // node_modules/d3-interpolate/src/numberArray.js
  function numberArray_default(a2, b) {
    if (!b)
      b = [];
    var n = a2 ? Math.min(b.length, a2.length) : 0, c3 = b.slice(), i;
    return function(t) {
      for (i = 0; i < n; ++i)
        c3[i] = a2[i] * (1 - t) + b[i] * t;
      return c3;
    };
  }
  function isNumberArray(x2) {
    return ArrayBuffer.isView(x2) && !(x2 instanceof DataView);
  }

  // node_modules/d3-interpolate/src/array.js
  function genericArray(a2, b) {
    var nb = b ? b.length : 0, na = a2 ? Math.min(nb, a2.length) : 0, x2 = new Array(na), c3 = new Array(nb), i;
    for (i = 0; i < na; ++i)
      x2[i] = value_default(a2[i], b[i]);
    for (; i < nb; ++i)
      c3[i] = b[i];
    return function(t) {
      for (i = 0; i < na; ++i)
        c3[i] = x2[i](t);
      return c3;
    };
  }

  // node_modules/d3-interpolate/src/date.js
  function date_default(a2, b) {
    var d = /* @__PURE__ */ new Date();
    return a2 = +a2, b = +b, function(t) {
      return d.setTime(a2 * (1 - t) + b * t), d;
    };
  }

  // node_modules/d3-interpolate/src/number.js
  function number_default(a2, b) {
    return a2 = +a2, b = +b, function(t) {
      return a2 * (1 - t) + b * t;
    };
  }

  // node_modules/d3-interpolate/src/object.js
  function object_default(a2, b) {
    var i = {}, c3 = {}, k;
    if (a2 === null || typeof a2 !== "object")
      a2 = {};
    if (b === null || typeof b !== "object")
      b = {};
    for (k in b) {
      if (k in a2) {
        i[k] = value_default(a2[k], b[k]);
      } else {
        c3[k] = b[k];
      }
    }
    return function(t) {
      for (k in i)
        c3[k] = i[k](t);
      return c3;
    };
  }

  // node_modules/d3-interpolate/src/string.js
  var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
  var reB = new RegExp(reA.source, "g");
  function zero2(b) {
    return function() {
      return b;
    };
  }
  function one(b) {
    return function(t) {
      return b(t) + "";
    };
  }
  function string_default(a2, b) {
    var bi = reA.lastIndex = reB.lastIndex = 0, am, bm, bs, i = -1, s = [], q = [];
    a2 = a2 + "", b = b + "";
    while ((am = reA.exec(a2)) && (bm = reB.exec(b))) {
      if ((bs = bm.index) > bi) {
        bs = b.slice(bi, bs);
        if (s[i])
          s[i] += bs;
        else
          s[++i] = bs;
      }
      if ((am = am[0]) === (bm = bm[0])) {
        if (s[i])
          s[i] += bm;
        else
          s[++i] = bm;
      } else {
        s[++i] = null;
        q.push({ i, x: number_default(am, bm) });
      }
      bi = reB.lastIndex;
    }
    if (bi < b.length) {
      bs = b.slice(bi);
      if (s[i])
        s[i] += bs;
      else
        s[++i] = bs;
    }
    return s.length < 2 ? q[0] ? one(q[0].x) : zero2(b) : (b = q.length, function(t) {
      for (var i2 = 0, o; i2 < b; ++i2)
        s[(o = q[i2]).i] = o.x(t);
      return s.join("");
    });
  }

  // node_modules/d3-interpolate/src/value.js
  function value_default(a2, b) {
    var t = typeof b, c3;
    return b == null || t === "boolean" ? constant_default2(b) : (t === "number" ? number_default : t === "string" ? (c3 = color(b)) ? (b = c3, rgb_default) : string_default : b instanceof color ? rgb_default : b instanceof Date ? date_default : isNumberArray(b) ? numberArray_default : Array.isArray(b) ? genericArray : typeof b.valueOf !== "function" && typeof b.toString !== "function" || isNaN(b) ? object_default : number_default)(a2, b);
  }

  // node_modules/d3-interpolate/src/round.js
  function round_default(a2, b) {
    return a2 = +a2, b = +b, function(t) {
      return Math.round(a2 * (1 - t) + b * t);
    };
  }

  // node_modules/d3-interpolate/src/transform/decompose.js
  var degrees2 = 180 / Math.PI;
  var identity2 = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1
  };
  function decompose_default(a2, b, c3, d, e, f) {
    var scaleX, scaleY, skewX;
    if (scaleX = Math.sqrt(a2 * a2 + b * b))
      a2 /= scaleX, b /= scaleX;
    if (skewX = a2 * c3 + b * d)
      c3 -= a2 * skewX, d -= b * skewX;
    if (scaleY = Math.sqrt(c3 * c3 + d * d))
      c3 /= scaleY, d /= scaleY, skewX /= scaleY;
    if (a2 * d < b * c3)
      a2 = -a2, b = -b, skewX = -skewX, scaleX = -scaleX;
    return {
      translateX: e,
      translateY: f,
      rotate: Math.atan2(b, a2) * degrees2,
      skewX: Math.atan(skewX) * degrees2,
      scaleX,
      scaleY
    };
  }

  // node_modules/d3-interpolate/src/transform/parse.js
  var svgNode;
  function parseCss(value) {
    const m2 = new (typeof DOMMatrix === "function" ? DOMMatrix : WebKitCSSMatrix)(value + "");
    return m2.isIdentity ? identity2 : decompose_default(m2.a, m2.b, m2.c, m2.d, m2.e, m2.f);
  }
  function parseSvg(value) {
    if (value == null)
      return identity2;
    if (!svgNode)
      svgNode = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svgNode.setAttribute("transform", value);
    if (!(value = svgNode.transform.baseVal.consolidate()))
      return identity2;
    value = value.matrix;
    return decompose_default(value.a, value.b, value.c, value.d, value.e, value.f);
  }

  // node_modules/d3-interpolate/src/transform/index.js
  function interpolateTransform(parse, pxComma, pxParen, degParen) {
    function pop(s) {
      return s.length ? s.pop() + " " : "";
    }
    function translate(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push("translate(", null, pxComma, null, pxParen);
        q.push({ i: i - 4, x: number_default(xa, xb) }, { i: i - 2, x: number_default(ya, yb) });
      } else if (xb || yb) {
        s.push("translate(" + xb + pxComma + yb + pxParen);
      }
    }
    function rotate(a2, b, s, q) {
      if (a2 !== b) {
        if (a2 - b > 180)
          b += 360;
        else if (b - a2 > 180)
          a2 += 360;
        q.push({ i: s.push(pop(s) + "rotate(", null, degParen) - 2, x: number_default(a2, b) });
      } else if (b) {
        s.push(pop(s) + "rotate(" + b + degParen);
      }
    }
    function skewX(a2, b, s, q) {
      if (a2 !== b) {
        q.push({ i: s.push(pop(s) + "skewX(", null, degParen) - 2, x: number_default(a2, b) });
      } else if (b) {
        s.push(pop(s) + "skewX(" + b + degParen);
      }
    }
    function scale(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push(pop(s) + "scale(", null, ",", null, ")");
        q.push({ i: i - 4, x: number_default(xa, xb) }, { i: i - 2, x: number_default(ya, yb) });
      } else if (xb !== 1 || yb !== 1) {
        s.push(pop(s) + "scale(" + xb + "," + yb + ")");
      }
    }
    return function(a2, b) {
      var s = [], q = [];
      a2 = parse(a2), b = parse(b);
      translate(a2.translateX, a2.translateY, b.translateX, b.translateY, s, q);
      rotate(a2.rotate, b.rotate, s, q);
      skewX(a2.skewX, b.skewX, s, q);
      scale(a2.scaleX, a2.scaleY, b.scaleX, b.scaleY, s, q);
      a2 = b = null;
      return function(t) {
        var i = -1, n = q.length, o;
        while (++i < n)
          s[(o = q[i]).i] = o.x(t);
        return s.join("");
      };
    };
  }
  var interpolateTransformCss = interpolateTransform(parseCss, "px, ", "px)", "deg)");
  var interpolateTransformSvg = interpolateTransform(parseSvg, ", ", ")", ")");

  // node_modules/d3-interpolate/src/cubehelix.js
  function cubehelix2(hue2) {
    return function cubehelixGamma(y2) {
      y2 = +y2;
      function cubehelix3(start2, end) {
        var h = hue2((start2 = cubehelix(start2)).h, (end = cubehelix(end)).h), s = nogamma(start2.s, end.s), l = nogamma(start2.l, end.l), opacity = nogamma(start2.opacity, end.opacity);
        return function(t) {
          start2.h = h(t);
          start2.s = s(t);
          start2.l = l(Math.pow(t, y2));
          start2.opacity = opacity(t);
          return start2 + "";
        };
      }
      cubehelix3.gamma = cubehelixGamma;
      return cubehelix3;
    }(1);
  }
  var cubehelix_default = cubehelix2(hue);
  var cubehelixLong = cubehelix2(nogamma);

  // node_modules/d3-timer/src/timer.js
  var frame = 0;
  var timeout = 0;
  var interval = 0;
  var pokeDelay = 1e3;
  var taskHead;
  var taskTail;
  var clockLast = 0;
  var clockNow = 0;
  var clockSkew = 0;
  var clock = typeof performance === "object" && performance.now ? performance : Date;
  var setFrame = typeof window === "object" && window.requestAnimationFrame ? window.requestAnimationFrame.bind(window) : function(f) {
    setTimeout(f, 17);
  };
  function now() {
    return clockNow || (setFrame(clearNow), clockNow = clock.now() + clockSkew);
  }
  function clearNow() {
    clockNow = 0;
  }
  function Timer() {
    this._call = this._time = this._next = null;
  }
  Timer.prototype = timer.prototype = {
    constructor: Timer,
    restart: function(callback, delay, time) {
      if (typeof callback !== "function")
        throw new TypeError("callback is not a function");
      time = (time == null ? now() : +time) + (delay == null ? 0 : +delay);
      if (!this._next && taskTail !== this) {
        if (taskTail)
          taskTail._next = this;
        else
          taskHead = this;
        taskTail = this;
      }
      this._call = callback;
      this._time = time;
      sleep();
    },
    stop: function() {
      if (this._call) {
        this._call = null;
        this._time = Infinity;
        sleep();
      }
    }
  };
  function timer(callback, delay, time) {
    var t = new Timer();
    t.restart(callback, delay, time);
    return t;
  }
  function timerFlush() {
    now();
    ++frame;
    var t = taskHead, e;
    while (t) {
      if ((e = clockNow - t._time) >= 0)
        t._call.call(void 0, e);
      t = t._next;
    }
    --frame;
  }
  function wake() {
    clockNow = (clockLast = clock.now()) + clockSkew;
    frame = timeout = 0;
    try {
      timerFlush();
    } finally {
      frame = 0;
      nap();
      clockNow = 0;
    }
  }
  function poke() {
    var now2 = clock.now(), delay = now2 - clockLast;
    if (delay > pokeDelay)
      clockSkew -= delay, clockLast = now2;
  }
  function nap() {
    var t0, t1 = taskHead, t2, time = Infinity;
    while (t1) {
      if (t1._call) {
        if (time > t1._time)
          time = t1._time;
        t0 = t1, t1 = t1._next;
      } else {
        t2 = t1._next, t1._next = null;
        t1 = t0 ? t0._next = t2 : taskHead = t2;
      }
    }
    taskTail = t0;
    sleep(time);
  }
  function sleep(time) {
    if (frame)
      return;
    if (timeout)
      timeout = clearTimeout(timeout);
    var delay = time - clockNow;
    if (delay > 24) {
      if (time < Infinity)
        timeout = setTimeout(wake, time - clock.now() - clockSkew);
      if (interval)
        interval = clearInterval(interval);
    } else {
      if (!interval)
        clockLast = clock.now(), interval = setInterval(poke, pokeDelay);
      frame = 1, setFrame(wake);
    }
  }

  // node_modules/d3-timer/src/timeout.js
  function timeout_default(callback, delay, time) {
    var t = new Timer();
    delay = delay == null ? 0 : +delay;
    t.restart((elapsed) => {
      t.stop();
      callback(elapsed + delay);
    }, delay, time);
    return t;
  }

  // node_modules/d3-transition/src/transition/schedule.js
  var emptyOn = dispatch_default("start", "end", "cancel", "interrupt");
  var emptyTween = [];
  var CREATED = 0;
  var SCHEDULED = 1;
  var STARTING = 2;
  var STARTED = 3;
  var RUNNING = 4;
  var ENDING = 5;
  var ENDED = 6;
  function schedule_default(node, name, id2, index, group, timing) {
    var schedules = node.__transition;
    if (!schedules)
      node.__transition = {};
    else if (id2 in schedules)
      return;
    create(node, id2, {
      name,
      index,
      // For context during callback.
      group,
      // For context during callback.
      on: emptyOn,
      tween: emptyTween,
      time: timing.time,
      delay: timing.delay,
      duration: timing.duration,
      ease: timing.ease,
      timer: null,
      state: CREATED
    });
  }
  function init(node, id2) {
    var schedule = get2(node, id2);
    if (schedule.state > CREATED)
      throw new Error("too late; already scheduled");
    return schedule;
  }
  function set2(node, id2) {
    var schedule = get2(node, id2);
    if (schedule.state > STARTED)
      throw new Error("too late; already running");
    return schedule;
  }
  function get2(node, id2) {
    var schedule = node.__transition;
    if (!schedule || !(schedule = schedule[id2]))
      throw new Error("transition not found");
    return schedule;
  }
  function create(node, id2, self) {
    var schedules = node.__transition, tween;
    schedules[id2] = self;
    self.timer = timer(schedule, 0, self.time);
    function schedule(elapsed) {
      self.state = SCHEDULED;
      self.timer.restart(start2, self.delay, self.time);
      if (self.delay <= elapsed)
        start2(elapsed - self.delay);
    }
    function start2(elapsed) {
      var i, j, n, o;
      if (self.state !== SCHEDULED)
        return stop();
      for (i in schedules) {
        o = schedules[i];
        if (o.name !== self.name)
          continue;
        if (o.state === STARTED)
          return timeout_default(start2);
        if (o.state === RUNNING) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("interrupt", node, node.__data__, o.index, o.group);
          delete schedules[i];
        } else if (+i < id2) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("cancel", node, node.__data__, o.index, o.group);
          delete schedules[i];
        }
      }
      timeout_default(function() {
        if (self.state === STARTED) {
          self.state = RUNNING;
          self.timer.restart(tick, self.delay, self.time);
          tick(elapsed);
        }
      });
      self.state = STARTING;
      self.on.call("start", node, node.__data__, self.index, self.group);
      if (self.state !== STARTING)
        return;
      self.state = STARTED;
      tween = new Array(n = self.tween.length);
      for (i = 0, j = -1; i < n; ++i) {
        if (o = self.tween[i].value.call(node, node.__data__, self.index, self.group)) {
          tween[++j] = o;
        }
      }
      tween.length = j + 1;
    }
    function tick(elapsed) {
      var t = elapsed < self.duration ? self.ease.call(null, elapsed / self.duration) : (self.timer.restart(stop), self.state = ENDING, 1), i = -1, n = tween.length;
      while (++i < n) {
        tween[i].call(node, t);
      }
      if (self.state === ENDING) {
        self.on.call("end", node, node.__data__, self.index, self.group);
        stop();
      }
    }
    function stop() {
      self.state = ENDED;
      self.timer.stop();
      delete schedules[id2];
      for (var i in schedules)
        return;
      delete node.__transition;
    }
  }

  // node_modules/d3-transition/src/interrupt.js
  function interrupt_default(node, name) {
    var schedules = node.__transition, schedule, active, empty3 = true, i;
    if (!schedules)
      return;
    name = name == null ? null : name + "";
    for (i in schedules) {
      if ((schedule = schedules[i]).name !== name) {
        empty3 = false;
        continue;
      }
      active = schedule.state > STARTING && schedule.state < ENDING;
      schedule.state = ENDED;
      schedule.timer.stop();
      schedule.on.call(active ? "interrupt" : "cancel", node, node.__data__, schedule.index, schedule.group);
      delete schedules[i];
    }
    if (empty3)
      delete node.__transition;
  }

  // node_modules/d3-transition/src/selection/interrupt.js
  function interrupt_default2(name) {
    return this.each(function() {
      interrupt_default(this, name);
    });
  }

  // node_modules/d3-transition/src/transition/tween.js
  function tweenRemove(id2, name) {
    var tween0, tween1;
    return function() {
      var schedule = set2(this, id2), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = tween0 = tween;
        for (var i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name) {
            tween1 = tween1.slice();
            tween1.splice(i, 1);
            break;
          }
        }
      }
      schedule.tween = tween1;
    };
  }
  function tweenFunction(id2, name, value) {
    var tween0, tween1;
    if (typeof value !== "function")
      throw new Error();
    return function() {
      var schedule = set2(this, id2), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = (tween0 = tween).slice();
        for (var t = { name, value }, i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name) {
            tween1[i] = t;
            break;
          }
        }
        if (i === n)
          tween1.push(t);
      }
      schedule.tween = tween1;
    };
  }
  function tween_default(name, value) {
    var id2 = this._id;
    name += "";
    if (arguments.length < 2) {
      var tween = get2(this.node(), id2).tween;
      for (var i = 0, n = tween.length, t; i < n; ++i) {
        if ((t = tween[i]).name === name) {
          return t.value;
        }
      }
      return null;
    }
    return this.each((value == null ? tweenRemove : tweenFunction)(id2, name, value));
  }
  function tweenValue(transition2, name, value) {
    var id2 = transition2._id;
    transition2.each(function() {
      var schedule = set2(this, id2);
      (schedule.value || (schedule.value = {}))[name] = value.apply(this, arguments);
    });
    return function(node) {
      return get2(node, id2).value[name];
    };
  }

  // node_modules/d3-transition/src/transition/interpolate.js
  function interpolate_default(a2, b) {
    var c3;
    return (typeof b === "number" ? number_default : b instanceof color ? rgb_default : (c3 = color(b)) ? (b = c3, rgb_default) : string_default)(a2, b);
  }

  // node_modules/d3-transition/src/transition/attr.js
  function attrRemove2(name) {
    return function() {
      this.removeAttribute(name);
    };
  }
  function attrRemoveNS2(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant2(name, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttribute(name);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrConstantNS2(fullname, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttributeNS(fullname.space, fullname.local);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrFunction2(name, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value(this), string1;
      if (value1 == null)
        return void this.removeAttribute(name);
      string0 = this.getAttribute(name);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attrFunctionNS2(fullname, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value(this), string1;
      if (value1 == null)
        return void this.removeAttributeNS(fullname.space, fullname.local);
      string0 = this.getAttributeNS(fullname.space, fullname.local);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attr_default2(name, value) {
    var fullname = namespace_default(name), i = fullname === "transform" ? interpolateTransformSvg : interpolate_default;
    return this.attrTween(name, typeof value === "function" ? (fullname.local ? attrFunctionNS2 : attrFunction2)(fullname, i, tweenValue(this, "attr." + name, value)) : value == null ? (fullname.local ? attrRemoveNS2 : attrRemove2)(fullname) : (fullname.local ? attrConstantNS2 : attrConstant2)(fullname, i, value));
  }

  // node_modules/d3-transition/src/transition/attrTween.js
  function attrInterpolate(name, i) {
    return function(t) {
      this.setAttribute(name, i.call(this, t));
    };
  }
  function attrInterpolateNS(fullname, i) {
    return function(t) {
      this.setAttributeNS(fullname.space, fullname.local, i.call(this, t));
    };
  }
  function attrTweenNS(fullname, value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolateNS(fullname, i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function attrTween(name, value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolate(name, i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function attrTween_default(name, value) {
    var key = "attr." + name;
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    var fullname = namespace_default(name);
    return this.tween(key, (fullname.local ? attrTweenNS : attrTween)(fullname, value));
  }

  // node_modules/d3-transition/src/transition/delay.js
  function delayFunction(id2, value) {
    return function() {
      init(this, id2).delay = +value.apply(this, arguments);
    };
  }
  function delayConstant(id2, value) {
    return value = +value, function() {
      init(this, id2).delay = value;
    };
  }
  function delay_default(value) {
    var id2 = this._id;
    return arguments.length ? this.each((typeof value === "function" ? delayFunction : delayConstant)(id2, value)) : get2(this.node(), id2).delay;
  }

  // node_modules/d3-transition/src/transition/duration.js
  function durationFunction(id2, value) {
    return function() {
      set2(this, id2).duration = +value.apply(this, arguments);
    };
  }
  function durationConstant(id2, value) {
    return value = +value, function() {
      set2(this, id2).duration = value;
    };
  }
  function duration_default(value) {
    var id2 = this._id;
    return arguments.length ? this.each((typeof value === "function" ? durationFunction : durationConstant)(id2, value)) : get2(this.node(), id2).duration;
  }

  // node_modules/d3-transition/src/transition/ease.js
  function easeConstant(id2, value) {
    if (typeof value !== "function")
      throw new Error();
    return function() {
      set2(this, id2).ease = value;
    };
  }
  function ease_default(value) {
    var id2 = this._id;
    return arguments.length ? this.each(easeConstant(id2, value)) : get2(this.node(), id2).ease;
  }

  // node_modules/d3-transition/src/transition/easeVarying.js
  function easeVarying(id2, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (typeof v !== "function")
        throw new Error();
      set2(this, id2).ease = v;
    };
  }
  function easeVarying_default(value) {
    if (typeof value !== "function")
      throw new Error();
    return this.each(easeVarying(this._id, value));
  }

  // node_modules/d3-transition/src/transition/filter.js
  function filter_default2(match) {
    if (typeof match !== "function")
      match = matcher_default(match);
    for (var groups = this._groups, m2 = groups.length, subgroups = new Array(m2), j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group[i]) && match.call(node, node.__data__, i, group)) {
          subgroup.push(node);
        }
      }
    }
    return new Transition(subgroups, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/merge.js
  function merge_default2(transition2) {
    if (transition2._id !== this._id)
      throw new Error();
    for (var groups0 = this._groups, groups1 = transition2._groups, m0 = groups0.length, m1 = groups1.length, m2 = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m2; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Transition(merges, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/on.js
  function start(name) {
    return (name + "").trim().split(/^|\s+/).every(function(t) {
      var i = t.indexOf(".");
      if (i >= 0)
        t = t.slice(0, i);
      return !t || t === "start";
    });
  }
  function onFunction(id2, name, listener) {
    var on0, on1, sit = start(name) ? init : set2;
    return function() {
      var schedule = sit(this, id2), on = schedule.on;
      if (on !== on0)
        (on1 = (on0 = on).copy()).on(name, listener);
      schedule.on = on1;
    };
  }
  function on_default2(name, listener) {
    var id2 = this._id;
    return arguments.length < 2 ? get2(this.node(), id2).on.on(name) : this.each(onFunction(id2, name, listener));
  }

  // node_modules/d3-transition/src/transition/remove.js
  function removeFunction(id2) {
    return function() {
      var parent = this.parentNode;
      for (var i in this.__transition)
        if (+i !== id2)
          return;
      if (parent)
        parent.removeChild(this);
    };
  }
  function remove_default2() {
    return this.on("end.remove", removeFunction(this._id));
  }

  // node_modules/d3-transition/src/transition/select.js
  function select_default3(select) {
    var name = this._name, id2 = this._id;
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m2 = groups.length, subgroups = new Array(m2), j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group[i]) && (subnode = select.call(node, node.__data__, i, group))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
          schedule_default(subgroup[i], name, id2, i, subgroup, get2(node, id2));
        }
      }
    }
    return new Transition(subgroups, this._parents, name, id2);
  }

  // node_modules/d3-transition/src/transition/selectAll.js
  function selectAll_default3(select) {
    var name = this._name, id2 = this._id;
    if (typeof select !== "function")
      select = selectorAll_default(select);
    for (var groups = this._groups, m2 = groups.length, subgroups = [], parents = [], j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          for (var children2 = select.call(node, node.__data__, i, group), child, inherit2 = get2(node, id2), k = 0, l = children2.length; k < l; ++k) {
            if (child = children2[k]) {
              schedule_default(child, name, id2, k, children2, inherit2);
            }
          }
          subgroups.push(children2);
          parents.push(node);
        }
      }
    }
    return new Transition(subgroups, parents, name, id2);
  }

  // node_modules/d3-transition/src/transition/selection.js
  var Selection2 = selection_default.prototype.constructor;
  function selection_default2() {
    return new Selection2(this._groups, this._parents);
  }

  // node_modules/d3-transition/src/transition/style.js
  function styleNull(name, interpolate) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name), string1 = (this.style.removeProperty(name), styleValue(this, name));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : interpolate0 = interpolate(string00 = string0, string10 = string1);
    };
  }
  function styleRemove2(name) {
    return function() {
      this.style.removeProperty(name);
    };
  }
  function styleConstant2(name, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = styleValue(this, name);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function styleFunction2(name, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name), value1 = value(this), string1 = value1 + "";
      if (value1 == null)
        string1 = value1 = (this.style.removeProperty(name), styleValue(this, name));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function styleMaybeRemove(id2, name) {
    var on0, on1, listener0, key = "style." + name, event = "end." + key, remove2;
    return function() {
      var schedule = set2(this, id2), on = schedule.on, listener = schedule.value[key] == null ? remove2 || (remove2 = styleRemove2(name)) : void 0;
      if (on !== on0 || listener0 !== listener)
        (on1 = (on0 = on).copy()).on(event, listener0 = listener);
      schedule.on = on1;
    };
  }
  function style_default2(name, value, priority) {
    var i = (name += "") === "transform" ? interpolateTransformCss : interpolate_default;
    return value == null ? this.styleTween(name, styleNull(name, i)).on("end.style." + name, styleRemove2(name)) : typeof value === "function" ? this.styleTween(name, styleFunction2(name, i, tweenValue(this, "style." + name, value))).each(styleMaybeRemove(this._id, name)) : this.styleTween(name, styleConstant2(name, i, value), priority).on("end.style." + name, null);
  }

  // node_modules/d3-transition/src/transition/styleTween.js
  function styleInterpolate(name, i, priority) {
    return function(t) {
      this.style.setProperty(name, i.call(this, t), priority);
    };
  }
  function styleTween(name, value, priority) {
    var t, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t = (i0 = i) && styleInterpolate(name, i, priority);
      return t;
    }
    tween._value = value;
    return tween;
  }
  function styleTween_default(name, value, priority) {
    var key = "style." + (name += "");
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    return this.tween(key, styleTween(name, value, priority == null ? "" : priority));
  }

  // node_modules/d3-transition/src/transition/text.js
  function textConstant2(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction2(value) {
    return function() {
      var value1 = value(this);
      this.textContent = value1 == null ? "" : value1;
    };
  }
  function text_default2(value) {
    return this.tween("text", typeof value === "function" ? textFunction2(tweenValue(this, "text", value)) : textConstant2(value == null ? "" : value + ""));
  }

  // node_modules/d3-transition/src/transition/textTween.js
  function textInterpolate(i) {
    return function(t) {
      this.textContent = i.call(this, t);
    };
  }
  function textTween(value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && textInterpolate(i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function textTween_default(value) {
    var key = "text";
    if (arguments.length < 1)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    return this.tween(key, textTween(value));
  }

  // node_modules/d3-transition/src/transition/transition.js
  function transition_default() {
    var name = this._name, id0 = this._id, id1 = newId();
    for (var groups = this._groups, m2 = groups.length, j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          var inherit2 = get2(node, id0);
          schedule_default(node, name, id1, i, group, {
            time: inherit2.time + inherit2.delay + inherit2.duration,
            delay: 0,
            duration: inherit2.duration,
            ease: inherit2.ease
          });
        }
      }
    }
    return new Transition(groups, this._parents, name, id1);
  }

  // node_modules/d3-transition/src/transition/end.js
  function end_default() {
    var on0, on1, that = this, id2 = that._id, size = that.size();
    return new Promise(function(resolve, reject) {
      var cancel = { value: reject }, end = { value: function() {
        if (--size === 0)
          resolve();
      } };
      that.each(function() {
        var schedule = set2(this, id2), on = schedule.on;
        if (on !== on0) {
          on1 = (on0 = on).copy();
          on1._.cancel.push(cancel);
          on1._.interrupt.push(cancel);
          on1._.end.push(end);
        }
        schedule.on = on1;
      });
      if (size === 0)
        resolve();
    });
  }

  // node_modules/d3-transition/src/transition/index.js
  var id = 0;
  function Transition(groups, parents, name, id2) {
    this._groups = groups;
    this._parents = parents;
    this._name = name;
    this._id = id2;
  }
  function transition(name) {
    return selection_default().transition(name);
  }
  function newId() {
    return ++id;
  }
  var selection_prototype = selection_default.prototype;
  Transition.prototype = transition.prototype = {
    constructor: Transition,
    select: select_default3,
    selectAll: selectAll_default3,
    selectChild: selection_prototype.selectChild,
    selectChildren: selection_prototype.selectChildren,
    filter: filter_default2,
    merge: merge_default2,
    selection: selection_default2,
    transition: transition_default,
    call: selection_prototype.call,
    nodes: selection_prototype.nodes,
    node: selection_prototype.node,
    size: selection_prototype.size,
    empty: selection_prototype.empty,
    each: selection_prototype.each,
    on: on_default2,
    attr: attr_default2,
    attrTween: attrTween_default,
    style: style_default2,
    styleTween: styleTween_default,
    text: text_default2,
    textTween: textTween_default,
    remove: remove_default2,
    tween: tween_default,
    delay: delay_default,
    duration: duration_default,
    ease: ease_default,
    easeVarying: easeVarying_default,
    end: end_default,
    [Symbol.iterator]: selection_prototype[Symbol.iterator]
  };

  // node_modules/d3-ease/src/cubic.js
  function cubicInOut(t) {
    return ((t *= 2) <= 1 ? t * t * t : (t -= 2) * t * t + 2) / 2;
  }

  // node_modules/d3-transition/src/selection/transition.js
  var defaultTiming = {
    time: null,
    // Set on use.
    delay: 0,
    duration: 250,
    ease: cubicInOut
  };
  function inherit(node, id2) {
    var timing;
    while (!(timing = node.__transition) || !(timing = timing[id2])) {
      if (!(node = node.parentNode)) {
        throw new Error(`transition ${id2} not found`);
      }
    }
    return timing;
  }
  function transition_default2(name) {
    var id2, timing;
    if (name instanceof Transition) {
      id2 = name._id, name = name._name;
    } else {
      id2 = newId(), (timing = defaultTiming).time = now(), name = name == null ? null : name + "";
    }
    for (var groups = this._groups, m2 = groups.length, j = 0; j < m2; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          schedule_default(node, name, id2, i, group, timing || inherit(node, id2));
        }
      }
    }
    return new Transition(groups, this._parents, name, id2);
  }

  // node_modules/d3-transition/src/selection/index.js
  selection_default.prototype.interrupt = interrupt_default2;
  selection_default.prototype.transition = transition_default2;

  // node_modules/d3-brush/src/constant.js
  var constant_default3 = (x2) => () => x2;

  // node_modules/d3-brush/src/event.js
  function BrushEvent(type2, {
    sourceEvent,
    target,
    selection: selection2,
    mode,
    dispatch: dispatch2
  }) {
    Object.defineProperties(this, {
      type: { value: type2, enumerable: true, configurable: true },
      sourceEvent: { value: sourceEvent, enumerable: true, configurable: true },
      target: { value: target, enumerable: true, configurable: true },
      selection: { value: selection2, enumerable: true, configurable: true },
      mode: { value: mode, enumerable: true, configurable: true },
      _: { value: dispatch2 }
    });
  }

  // node_modules/d3-brush/src/noevent.js
  function nopropagation(event) {
    event.stopImmediatePropagation();
  }
  function noevent_default2(event) {
    event.preventDefault();
    event.stopImmediatePropagation();
  }

  // node_modules/d3-brush/src/brush.js
  var MODE_DRAG = { name: "drag" };
  var MODE_SPACE = { name: "space" };
  var MODE_HANDLE = { name: "handle" };
  var MODE_CENTER = { name: "center" };
  var { abs, max: max2, min: min2 } = Math;
  function number1(e) {
    return [+e[0], +e[1]];
  }
  function number22(e) {
    return [number1(e[0]), number1(e[1])];
  }
  var X = {
    name: "x",
    handles: ["w", "e"].map(type),
    input: function(x2, e) {
      return x2 == null ? null : [[+x2[0], e[0][1]], [+x2[1], e[1][1]]];
    },
    output: function(xy) {
      return xy && [xy[0][0], xy[1][0]];
    }
  };
  var Y = {
    name: "y",
    handles: ["n", "s"].map(type),
    input: function(y2, e) {
      return y2 == null ? null : [[e[0][0], +y2[0]], [e[1][0], +y2[1]]];
    },
    output: function(xy) {
      return xy && [xy[0][1], xy[1][1]];
    }
  };
  var XY = {
    name: "xy",
    handles: ["n", "w", "e", "s", "nw", "ne", "sw", "se"].map(type),
    input: function(xy) {
      return xy == null ? null : number22(xy);
    },
    output: function(xy) {
      return xy;
    }
  };
  var cursors = {
    overlay: "crosshair",
    selection: "move",
    n: "ns-resize",
    e: "ew-resize",
    s: "ns-resize",
    w: "ew-resize",
    nw: "nwse-resize",
    ne: "nesw-resize",
    se: "nwse-resize",
    sw: "nesw-resize"
  };
  var flipX = {
    e: "w",
    w: "e",
    nw: "ne",
    ne: "nw",
    se: "sw",
    sw: "se"
  };
  var flipY = {
    n: "s",
    s: "n",
    nw: "sw",
    ne: "se",
    se: "ne",
    sw: "nw"
  };
  var signsX = {
    overlay: 1,
    selection: 1,
    n: null,
    e: 1,
    s: null,
    w: -1,
    nw: -1,
    ne: 1,
    se: 1,
    sw: -1
  };
  var signsY = {
    overlay: 1,
    selection: 1,
    n: -1,
    e: null,
    s: 1,
    w: null,
    nw: -1,
    ne: -1,
    se: 1,
    sw: 1
  };
  function type(t) {
    return { type: t };
  }
  function defaultFilter(event) {
    return !event.ctrlKey && !event.button;
  }
  function defaultExtent() {
    var svg = this.ownerSVGElement || this;
    if (svg.hasAttribute("viewBox")) {
      svg = svg.viewBox.baseVal;
      return [[svg.x, svg.y], [svg.x + svg.width, svg.y + svg.height]];
    }
    return [[0, 0], [svg.width.baseVal.value, svg.height.baseVal.value]];
  }
  function defaultTouchable() {
    return navigator.maxTouchPoints || "ontouchstart" in this;
  }
  function local(node) {
    while (!node.__brush)
      if (!(node = node.parentNode))
        return;
    return node.__brush;
  }
  function empty2(extent2) {
    return extent2[0][0] === extent2[1][0] || extent2[0][1] === extent2[1][1];
  }
  function brushSelection(node) {
    var state = node.__brush;
    return state ? state.dim.output(state.selection) : null;
  }
  function brushX() {
    return brush(X);
  }
  function brushY() {
    return brush(Y);
  }
  function brush_default() {
    return brush(XY);
  }
  function brush(dim) {
    var extent2 = defaultExtent, filter2 = defaultFilter, touchable = defaultTouchable, keys = true, listeners = dispatch_default("start", "brush", "end"), handleSize = 6, touchending;
    function brush2(group) {
      var overlay = group.property("__brush", initialize).selectAll(".overlay").data([type("overlay")]);
      overlay.enter().append("rect").attr("class", "overlay").attr("pointer-events", "all").attr("cursor", cursors.overlay).merge(overlay).each(function() {
        var extent3 = local(this).extent;
        select_default2(this).attr("x", extent3[0][0]).attr("y", extent3[0][1]).attr("width", extent3[1][0] - extent3[0][0]).attr("height", extent3[1][1] - extent3[0][1]);
      });
      group.selectAll(".selection").data([type("selection")]).enter().append("rect").attr("class", "selection").attr("cursor", cursors.selection).attr("fill", "#777").attr("fill-opacity", 0.3).attr("stroke", "#fff").attr("shape-rendering", "crispEdges");
      var handle = group.selectAll(".handle").data(dim.handles, function(d) {
        return d.type;
      });
      handle.exit().remove();
      handle.enter().append("rect").attr("class", function(d) {
        return "handle handle--" + d.type;
      }).attr("cursor", function(d) {
        return cursors[d.type];
      });
      group.each(redraw).attr("fill", "none").attr("pointer-events", "all").on("mousedown.brush", started).filter(touchable).on("touchstart.brush", started).on("touchmove.brush", touchmoved).on("touchend.brush touchcancel.brush", touchended).style("touch-action", "none").style("-webkit-tap-highlight-color", "rgba(0,0,0,0)");
    }
    brush2.move = function(group, selection2, event) {
      if (group.tween) {
        group.on("start.brush", function(event2) {
          emitter(this, arguments).beforestart().start(event2);
        }).on("interrupt.brush end.brush", function(event2) {
          emitter(this, arguments).end(event2);
        }).tween("brush", function() {
          var that = this, state = that.__brush, emit = emitter(that, arguments), selection0 = state.selection, selection1 = dim.input(typeof selection2 === "function" ? selection2.apply(this, arguments) : selection2, state.extent), i = value_default(selection0, selection1);
          function tween(t) {
            state.selection = t === 1 && selection1 === null ? null : i(t);
            redraw.call(that);
            emit.brush();
          }
          return selection0 !== null && selection1 !== null ? tween : tween(1);
        });
      } else {
        group.each(function() {
          var that = this, args = arguments, state = that.__brush, selection1 = dim.input(typeof selection2 === "function" ? selection2.apply(that, args) : selection2, state.extent), emit = emitter(that, args).beforestart();
          interrupt_default(that);
          state.selection = selection1 === null ? null : selection1;
          redraw.call(that);
          emit.start(event).brush(event).end(event);
        });
      }
    };
    brush2.clear = function(group, event) {
      brush2.move(group, null, event);
    };
    function redraw() {
      var group = select_default2(this), selection2 = local(this).selection;
      if (selection2) {
        group.selectAll(".selection").style("display", null).attr("x", selection2[0][0]).attr("y", selection2[0][1]).attr("width", selection2[1][0] - selection2[0][0]).attr("height", selection2[1][1] - selection2[0][1]);
        group.selectAll(".handle").style("display", null).attr("x", function(d) {
          return d.type[d.type.length - 1] === "e" ? selection2[1][0] - handleSize / 2 : selection2[0][0] - handleSize / 2;
        }).attr("y", function(d) {
          return d.type[0] === "s" ? selection2[1][1] - handleSize / 2 : selection2[0][1] - handleSize / 2;
        }).attr("width", function(d) {
          return d.type === "n" || d.type === "s" ? selection2[1][0] - selection2[0][0] + handleSize : handleSize;
        }).attr("height", function(d) {
          return d.type === "e" || d.type === "w" ? selection2[1][1] - selection2[0][1] + handleSize : handleSize;
        });
      } else {
        group.selectAll(".selection,.handle").style("display", "none").attr("x", null).attr("y", null).attr("width", null).attr("height", null);
      }
    }
    function emitter(that, args, clean) {
      var emit = that.__brush.emitter;
      return emit && (!clean || !emit.clean) ? emit : new Emitter(that, args, clean);
    }
    function Emitter(that, args, clean) {
      this.that = that;
      this.args = args;
      this.state = that.__brush;
      this.active = 0;
      this.clean = clean;
    }
    Emitter.prototype = {
      beforestart: function() {
        if (++this.active === 1)
          this.state.emitter = this, this.starting = true;
        return this;
      },
      start: function(event, mode) {
        if (this.starting)
          this.starting = false, this.emit("start", event, mode);
        else
          this.emit("brush", event);
        return this;
      },
      brush: function(event, mode) {
        this.emit("brush", event, mode);
        return this;
      },
      end: function(event, mode) {
        if (--this.active === 0)
          delete this.state.emitter, this.emit("end", event, mode);
        return this;
      },
      emit: function(type2, event, mode) {
        var d = select_default2(this.that).datum();
        listeners.call(
          type2,
          this.that,
          new BrushEvent(type2, {
            sourceEvent: event,
            target: brush2,
            selection: dim.output(this.state.selection),
            mode,
            dispatch: listeners
          }),
          d
        );
      }
    };
    function started(event) {
      if (touchending && !event.touches)
        return;
      if (!filter2.apply(this, arguments))
        return;
      var that = this, type2 = event.target.__data__.type, mode = (keys && event.metaKey ? type2 = "overlay" : type2) === "selection" ? MODE_DRAG : keys && event.altKey ? MODE_CENTER : MODE_HANDLE, signX = dim === Y ? null : signsX[type2], signY = dim === X ? null : signsY[type2], state = local(that), extent3 = state.extent, selection2 = state.selection, W = extent3[0][0], w0, w1, N = extent3[0][1], n0, n1, E2 = extent3[1][0], e0, e1, S = extent3[1][1], s0, s1, dx = 0, dy = 0, moving, shifting = signX && signY && keys && event.shiftKey, lockX, lockY, points2 = Array.from(event.touches || [event], (t) => {
        const i = t.identifier;
        t = pointer_default(t, that);
        t.point0 = t.slice();
        t.identifier = i;
        return t;
      });
      interrupt_default(that);
      var emit = emitter(that, arguments, true).beforestart();
      if (type2 === "overlay") {
        if (selection2)
          moving = true;
        const pts = [points2[0], points2[1] || points2[0]];
        state.selection = selection2 = [[
          w0 = dim === Y ? W : min2(pts[0][0], pts[1][0]),
          n0 = dim === X ? N : min2(pts[0][1], pts[1][1])
        ], [
          e0 = dim === Y ? E2 : max2(pts[0][0], pts[1][0]),
          s0 = dim === X ? S : max2(pts[0][1], pts[1][1])
        ]];
        if (points2.length > 1)
          move(event);
      } else {
        w0 = selection2[0][0];
        n0 = selection2[0][1];
        e0 = selection2[1][0];
        s0 = selection2[1][1];
      }
      w1 = w0;
      n1 = n0;
      e1 = e0;
      s1 = s0;
      var group = select_default2(that).attr("pointer-events", "none");
      var overlay = group.selectAll(".overlay").attr("cursor", cursors[type2]);
      if (event.touches) {
        emit.moved = moved;
        emit.ended = ended;
      } else {
        var view = select_default2(event.view).on("mousemove.brush", moved, true).on("mouseup.brush", ended, true);
        if (keys)
          view.on("keydown.brush", keydowned, true).on("keyup.brush", keyupped, true);
        nodrag_default(event.view);
      }
      redraw.call(that);
      emit.start(event, mode.name);
      function moved(event2) {
        for (const p of event2.changedTouches || [event2]) {
          for (const d of points2)
            if (d.identifier === p.identifier)
              d.cur = pointer_default(p, that);
        }
        if (shifting && !lockX && !lockY && points2.length === 1) {
          const point3 = points2[0];
          if (abs(point3.cur[0] - point3[0]) > abs(point3.cur[1] - point3[1]))
            lockY = true;
          else
            lockX = true;
        }
        for (const point3 of points2)
          if (point3.cur)
            point3[0] = point3.cur[0], point3[1] = point3.cur[1];
        moving = true;
        noevent_default2(event2);
        move(event2);
      }
      function move(event2) {
        const point3 = points2[0], point0 = point3.point0;
        var t;
        dx = point3[0] - point0[0];
        dy = point3[1] - point0[1];
        switch (mode) {
          case MODE_SPACE:
          case MODE_DRAG: {
            if (signX)
              dx = max2(W - w0, min2(E2 - e0, dx)), w1 = w0 + dx, e1 = e0 + dx;
            if (signY)
              dy = max2(N - n0, min2(S - s0, dy)), n1 = n0 + dy, s1 = s0 + dy;
            break;
          }
          case MODE_HANDLE: {
            if (points2[1]) {
              if (signX)
                w1 = max2(W, min2(E2, points2[0][0])), e1 = max2(W, min2(E2, points2[1][0])), signX = 1;
              if (signY)
                n1 = max2(N, min2(S, points2[0][1])), s1 = max2(N, min2(S, points2[1][1])), signY = 1;
            } else {
              if (signX < 0)
                dx = max2(W - w0, min2(E2 - w0, dx)), w1 = w0 + dx, e1 = e0;
              else if (signX > 0)
                dx = max2(W - e0, min2(E2 - e0, dx)), w1 = w0, e1 = e0 + dx;
              if (signY < 0)
                dy = max2(N - n0, min2(S - n0, dy)), n1 = n0 + dy, s1 = s0;
              else if (signY > 0)
                dy = max2(N - s0, min2(S - s0, dy)), n1 = n0, s1 = s0 + dy;
            }
            break;
          }
          case MODE_CENTER: {
            if (signX)
              w1 = max2(W, min2(E2, w0 - dx * signX)), e1 = max2(W, min2(E2, e0 + dx * signX));
            if (signY)
              n1 = max2(N, min2(S, n0 - dy * signY)), s1 = max2(N, min2(S, s0 + dy * signY));
            break;
          }
        }
        if (e1 < w1) {
          signX *= -1;
          t = w0, w0 = e0, e0 = t;
          t = w1, w1 = e1, e1 = t;
          if (type2 in flipX)
            overlay.attr("cursor", cursors[type2 = flipX[type2]]);
        }
        if (s1 < n1) {
          signY *= -1;
          t = n0, n0 = s0, s0 = t;
          t = n1, n1 = s1, s1 = t;
          if (type2 in flipY)
            overlay.attr("cursor", cursors[type2 = flipY[type2]]);
        }
        if (state.selection)
          selection2 = state.selection;
        if (lockX)
          w1 = selection2[0][0], e1 = selection2[1][0];
        if (lockY)
          n1 = selection2[0][1], s1 = selection2[1][1];
        if (selection2[0][0] !== w1 || selection2[0][1] !== n1 || selection2[1][0] !== e1 || selection2[1][1] !== s1) {
          state.selection = [[w1, n1], [e1, s1]];
          redraw.call(that);
          emit.brush(event2, mode.name);
        }
      }
      function ended(event2) {
        nopropagation(event2);
        if (event2.touches) {
          if (event2.touches.length)
            return;
          if (touchending)
            clearTimeout(touchending);
          touchending = setTimeout(function() {
            touchending = null;
          }, 500);
        } else {
          yesdrag(event2.view, moving);
          view.on("keydown.brush keyup.brush mousemove.brush mouseup.brush", null);
        }
        group.attr("pointer-events", "all");
        overlay.attr("cursor", cursors.overlay);
        if (state.selection)
          selection2 = state.selection;
        if (empty2(selection2))
          state.selection = null, redraw.call(that);
        emit.end(event2, mode.name);
      }
      function keydowned(event2) {
        switch (event2.keyCode) {
          case 16: {
            shifting = signX && signY;
            break;
          }
          case 18: {
            if (mode === MODE_HANDLE) {
              if (signX)
                e0 = e1 - dx * signX, w0 = w1 + dx * signX;
              if (signY)
                s0 = s1 - dy * signY, n0 = n1 + dy * signY;
              mode = MODE_CENTER;
              move(event2);
            }
            break;
          }
          case 32: {
            if (mode === MODE_HANDLE || mode === MODE_CENTER) {
              if (signX < 0)
                e0 = e1 - dx;
              else if (signX > 0)
                w0 = w1 - dx;
              if (signY < 0)
                s0 = s1 - dy;
              else if (signY > 0)
                n0 = n1 - dy;
              mode = MODE_SPACE;
              overlay.attr("cursor", cursors.selection);
              move(event2);
            }
            break;
          }
          default:
            return;
        }
        noevent_default2(event2);
      }
      function keyupped(event2) {
        switch (event2.keyCode) {
          case 16: {
            if (shifting) {
              lockX = lockY = shifting = false;
              move(event2);
            }
            break;
          }
          case 18: {
            if (mode === MODE_CENTER) {
              if (signX < 0)
                e0 = e1;
              else if (signX > 0)
                w0 = w1;
              if (signY < 0)
                s0 = s1;
              else if (signY > 0)
                n0 = n1;
              mode = MODE_HANDLE;
              move(event2);
            }
            break;
          }
          case 32: {
            if (mode === MODE_SPACE) {
              if (event2.altKey) {
                if (signX)
                  e0 = e1 - dx * signX, w0 = w1 + dx * signX;
                if (signY)
                  s0 = s1 - dy * signY, n0 = n1 + dy * signY;
                mode = MODE_CENTER;
              } else {
                if (signX < 0)
                  e0 = e1;
                else if (signX > 0)
                  w0 = w1;
                if (signY < 0)
                  s0 = s1;
                else if (signY > 0)
                  n0 = n1;
                mode = MODE_HANDLE;
              }
              overlay.attr("cursor", cursors[type2]);
              move(event2);
            }
            break;
          }
          default:
            return;
        }
        noevent_default2(event2);
      }
    }
    function touchmoved(event) {
      emitter(this, arguments).moved(event);
    }
    function touchended(event) {
      emitter(this, arguments).ended(event);
    }
    function initialize() {
      var state = this.__brush || { selection: null };
      state.extent = number22(extent2.apply(this, arguments));
      state.dim = dim;
      return state;
    }
    brush2.extent = function(_) {
      return arguments.length ? (extent2 = typeof _ === "function" ? _ : constant_default3(number22(_)), brush2) : extent2;
    };
    brush2.filter = function(_) {
      return arguments.length ? (filter2 = typeof _ === "function" ? _ : constant_default3(!!_), brush2) : filter2;
    };
    brush2.touchable = function(_) {
      return arguments.length ? (touchable = typeof _ === "function" ? _ : constant_default3(!!_), brush2) : touchable;
    };
    brush2.handleSize = function(_) {
      return arguments.length ? (handleSize = +_, brush2) : handleSize;
    };
    brush2.keyModifiers = function(_) {
      return arguments.length ? (keys = !!_, brush2) : keys;
    };
    brush2.on = function() {
      var value = listeners.on.apply(listeners, arguments);
      return value === listeners ? brush2 : value;
    };
    return brush2;
  }

  // node_modules/d3-path/src/path.js
  var pi = Math.PI;
  var tau = 2 * pi;
  var epsilon2 = 1e-6;
  var tauEpsilon = tau - epsilon2;
  function append(strings) {
    this._ += strings[0];
    for (let i = 1, n = strings.length; i < n; ++i) {
      this._ += arguments[i] + strings[i];
    }
  }
  function appendRound(digits) {
    let d = Math.floor(digits);
    if (!(d >= 0))
      throw new Error(`invalid digits: ${digits}`);
    if (d > 15)
      return append;
    const k = 10 ** d;
    return function(strings) {
      this._ += strings[0];
      for (let i = 1, n = strings.length; i < n; ++i) {
        this._ += Math.round(arguments[i] * k) / k + strings[i];
      }
    };
  }
  var Path = class {
    constructor(digits) {
      this._x0 = this._y0 = // start of current subpath
      this._x1 = this._y1 = null;
      this._ = "";
      this._append = digits == null ? append : appendRound(digits);
    }
    moveTo(x2, y2) {
      this._append`M${this._x0 = this._x1 = +x2},${this._y0 = this._y1 = +y2}`;
    }
    closePath() {
      if (this._x1 !== null) {
        this._x1 = this._x0, this._y1 = this._y0;
        this._append`Z`;
      }
    }
    lineTo(x2, y2) {
      this._append`L${this._x1 = +x2},${this._y1 = +y2}`;
    }
    quadraticCurveTo(x1, y1, x2, y2) {
      this._append`Q${+x1},${+y1},${this._x1 = +x2},${this._y1 = +y2}`;
    }
    bezierCurveTo(x1, y1, x2, y2, x3, y3) {
      this._append`C${+x1},${+y1},${+x2},${+y2},${this._x1 = +x3},${this._y1 = +y3}`;
    }
    arcTo(x1, y1, x2, y2, r) {
      x1 = +x1, y1 = +y1, x2 = +x2, y2 = +y2, r = +r;
      if (r < 0)
        throw new Error(`negative radius: ${r}`);
      let x0 = this._x1, y0 = this._y1, x21 = x2 - x1, y21 = y2 - y1, x01 = x0 - x1, y01 = y0 - y1, l01_2 = x01 * x01 + y01 * y01;
      if (this._x1 === null) {
        this._append`M${this._x1 = x1},${this._y1 = y1}`;
      } else if (!(l01_2 > epsilon2))
        ;
      else if (!(Math.abs(y01 * x21 - y21 * x01) > epsilon2) || !r) {
        this._append`L${this._x1 = x1},${this._y1 = y1}`;
      } else {
        let x20 = x2 - x0, y20 = y2 - y0, l21_2 = x21 * x21 + y21 * y21, l20_2 = x20 * x20 + y20 * y20, l21 = Math.sqrt(l21_2), l01 = Math.sqrt(l01_2), l = r * Math.tan((pi - Math.acos((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2), t01 = l / l01, t21 = l / l21;
        if (Math.abs(t01 - 1) > epsilon2) {
          this._append`L${x1 + t01 * x01},${y1 + t01 * y01}`;
        }
        this._append`A${r},${r},0,0,${+(y01 * x20 > x01 * y20)},${this._x1 = x1 + t21 * x21},${this._y1 = y1 + t21 * y21}`;
      }
    }
    arc(x2, y2, r, a0, a1, ccw) {
      x2 = +x2, y2 = +y2, r = +r, ccw = !!ccw;
      if (r < 0)
        throw new Error(`negative radius: ${r}`);
      let dx = r * Math.cos(a0), dy = r * Math.sin(a0), x0 = x2 + dx, y0 = y2 + dy, cw = 1 ^ ccw, da = ccw ? a0 - a1 : a1 - a0;
      if (this._x1 === null) {
        this._append`M${x0},${y0}`;
      } else if (Math.abs(this._x1 - x0) > epsilon2 || Math.abs(this._y1 - y0) > epsilon2) {
        this._append`L${x0},${y0}`;
      }
      if (!r)
        return;
      if (da < 0)
        da = da % tau + tau;
      if (da > tauEpsilon) {
        this._append`A${r},${r},0,1,${cw},${x2 - dx},${y2 - dy}A${r},${r},0,1,${cw},${this._x1 = x0},${this._y1 = y0}`;
      } else if (da > epsilon2) {
        this._append`A${r},${r},0,${+(da >= pi)},${cw},${this._x1 = x2 + r * Math.cos(a1)},${this._y1 = y2 + r * Math.sin(a1)}`;
      }
    }
    rect(x2, y2, w, h) {
      this._append`M${this._x0 = this._x1 = +x2},${this._y0 = this._y1 = +y2}h${w = +w}v${+h}h${-w}Z`;
    }
    toString() {
      return this._;
    }
  };
  function path() {
    return new Path();
  }
  path.prototype = Path.prototype;

  // node_modules/d3-format/src/formatDecimal.js
  function formatDecimal_default(x2) {
    return Math.abs(x2 = Math.round(x2)) >= 1e21 ? x2.toLocaleString("en").replace(/,/g, "") : x2.toString(10);
  }
  function formatDecimalParts(x2, p) {
    if ((i = (x2 = p ? x2.toExponential(p - 1) : x2.toExponential()).indexOf("e")) < 0)
      return null;
    var i, coefficient = x2.slice(0, i);
    return [
      coefficient.length > 1 ? coefficient[0] + coefficient.slice(2) : coefficient,
      +x2.slice(i + 1)
    ];
  }

  // node_modules/d3-format/src/exponent.js
  function exponent_default(x2) {
    return x2 = formatDecimalParts(Math.abs(x2)), x2 ? x2[1] : NaN;
  }

  // node_modules/d3-format/src/formatGroup.js
  function formatGroup_default(grouping, thousands) {
    return function(value, width) {
      var i = value.length, t = [], j = 0, g = grouping[0], length = 0;
      while (i > 0 && g > 0) {
        if (length + g + 1 > width)
          g = Math.max(1, width - length);
        t.push(value.substring(i -= g, i + g));
        if ((length += g + 1) > width)
          break;
        g = grouping[j = (j + 1) % grouping.length];
      }
      return t.reverse().join(thousands);
    };
  }

  // node_modules/d3-format/src/formatNumerals.js
  function formatNumerals_default(numerals) {
    return function(value) {
      return value.replace(/[0-9]/g, function(i) {
        return numerals[+i];
      });
    };
  }

  // node_modules/d3-format/src/formatSpecifier.js
  var re = /^(?:(.)?([<>=^]))?([+\-( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?(~)?([a-z%])?$/i;
  function formatSpecifier(specifier) {
    if (!(match = re.exec(specifier)))
      throw new Error("invalid format: " + specifier);
    var match;
    return new FormatSpecifier({
      fill: match[1],
      align: match[2],
      sign: match[3],
      symbol: match[4],
      zero: match[5],
      width: match[6],
      comma: match[7],
      precision: match[8] && match[8].slice(1),
      trim: match[9],
      type: match[10]
    });
  }
  formatSpecifier.prototype = FormatSpecifier.prototype;
  function FormatSpecifier(specifier) {
    this.fill = specifier.fill === void 0 ? " " : specifier.fill + "";
    this.align = specifier.align === void 0 ? ">" : specifier.align + "";
    this.sign = specifier.sign === void 0 ? "-" : specifier.sign + "";
    this.symbol = specifier.symbol === void 0 ? "" : specifier.symbol + "";
    this.zero = !!specifier.zero;
    this.width = specifier.width === void 0 ? void 0 : +specifier.width;
    this.comma = !!specifier.comma;
    this.precision = specifier.precision === void 0 ? void 0 : +specifier.precision;
    this.trim = !!specifier.trim;
    this.type = specifier.type === void 0 ? "" : specifier.type + "";
  }
  FormatSpecifier.prototype.toString = function() {
    return this.fill + this.align + this.sign + this.symbol + (this.zero ? "0" : "") + (this.width === void 0 ? "" : Math.max(1, this.width | 0)) + (this.comma ? "," : "") + (this.precision === void 0 ? "" : "." + Math.max(0, this.precision | 0)) + (this.trim ? "~" : "") + this.type;
  };

  // node_modules/d3-format/src/formatTrim.js
  function formatTrim_default(s) {
    out:
      for (var n = s.length, i = 1, i0 = -1, i1; i < n; ++i) {
        switch (s[i]) {
          case ".":
            i0 = i1 = i;
            break;
          case "0":
            if (i0 === 0)
              i0 = i;
            i1 = i;
            break;
          default:
            if (!+s[i])
              break out;
            if (i0 > 0)
              i0 = 0;
            break;
        }
      }
    return i0 > 0 ? s.slice(0, i0) + s.slice(i1 + 1) : s;
  }

  // node_modules/d3-format/src/formatPrefixAuto.js
  var prefixExponent;
  function formatPrefixAuto_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1], i = exponent - (prefixExponent = Math.max(-8, Math.min(8, Math.floor(exponent / 3))) * 3) + 1, n = coefficient.length;
    return i === n ? coefficient : i > n ? coefficient + new Array(i - n + 1).join("0") : i > 0 ? coefficient.slice(0, i) + "." + coefficient.slice(i) : "0." + new Array(1 - i).join("0") + formatDecimalParts(x2, Math.max(0, p + i - 1))[0];
  }

  // node_modules/d3-format/src/formatRounded.js
  function formatRounded_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1];
    return exponent < 0 ? "0." + new Array(-exponent).join("0") + coefficient : coefficient.length > exponent + 1 ? coefficient.slice(0, exponent + 1) + "." + coefficient.slice(exponent + 1) : coefficient + new Array(exponent - coefficient.length + 2).join("0");
  }

  // node_modules/d3-format/src/formatTypes.js
  var formatTypes_default = {
    "%": (x2, p) => (x2 * 100).toFixed(p),
    "b": (x2) => Math.round(x2).toString(2),
    "c": (x2) => x2 + "",
    "d": formatDecimal_default,
    "e": (x2, p) => x2.toExponential(p),
    "f": (x2, p) => x2.toFixed(p),
    "g": (x2, p) => x2.toPrecision(p),
    "o": (x2) => Math.round(x2).toString(8),
    "p": (x2, p) => formatRounded_default(x2 * 100, p),
    "r": formatRounded_default,
    "s": formatPrefixAuto_default,
    "X": (x2) => Math.round(x2).toString(16).toUpperCase(),
    "x": (x2) => Math.round(x2).toString(16)
  };

  // node_modules/d3-format/src/identity.js
  function identity_default2(x2) {
    return x2;
  }

  // node_modules/d3-format/src/locale.js
  var map2 = Array.prototype.map;
  var prefixes = ["y", "z", "a", "f", "p", "n", "\xB5", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y"];
  function locale_default(locale2) {
    var group = locale2.grouping === void 0 || locale2.thousands === void 0 ? identity_default2 : formatGroup_default(map2.call(locale2.grouping, Number), locale2.thousands + ""), currencyPrefix = locale2.currency === void 0 ? "" : locale2.currency[0] + "", currencySuffix = locale2.currency === void 0 ? "" : locale2.currency[1] + "", decimal = locale2.decimal === void 0 ? "." : locale2.decimal + "", numerals = locale2.numerals === void 0 ? identity_default2 : formatNumerals_default(map2.call(locale2.numerals, String)), percent = locale2.percent === void 0 ? "%" : locale2.percent + "", minus = locale2.minus === void 0 ? "\u2212" : locale2.minus + "", nan = locale2.nan === void 0 ? "NaN" : locale2.nan + "";
    function newFormat(specifier) {
      specifier = formatSpecifier(specifier);
      var fill = specifier.fill, align = specifier.align, sign = specifier.sign, symbol = specifier.symbol, zero3 = specifier.zero, width = specifier.width, comma = specifier.comma, precision = specifier.precision, trim = specifier.trim, type2 = specifier.type;
      if (type2 === "n")
        comma = true, type2 = "g";
      else if (!formatTypes_default[type2])
        precision === void 0 && (precision = 12), trim = true, type2 = "g";
      if (zero3 || fill === "0" && align === "=")
        zero3 = true, fill = "0", align = "=";
      var prefix = symbol === "$" ? currencyPrefix : symbol === "#" && /[boxX]/.test(type2) ? "0" + type2.toLowerCase() : "", suffix = symbol === "$" ? currencySuffix : /[%p]/.test(type2) ? percent : "";
      var formatType = formatTypes_default[type2], maybeSuffix = /[defgprs%]/.test(type2);
      precision = precision === void 0 ? 6 : /[gprs]/.test(type2) ? Math.max(1, Math.min(21, precision)) : Math.max(0, Math.min(20, precision));
      function format2(value) {
        var valuePrefix = prefix, valueSuffix = suffix, i, n, c3;
        if (type2 === "c") {
          valueSuffix = formatType(value) + valueSuffix;
          value = "";
        } else {
          value = +value;
          var valueNegative = value < 0 || 1 / value < 0;
          value = isNaN(value) ? nan : formatType(Math.abs(value), precision);
          if (trim)
            value = formatTrim_default(value);
          if (valueNegative && +value === 0 && sign !== "+")
            valueNegative = false;
          valuePrefix = (valueNegative ? sign === "(" ? sign : minus : sign === "-" || sign === "(" ? "" : sign) + valuePrefix;
          valueSuffix = (type2 === "s" ? prefixes[8 + prefixExponent / 3] : "") + valueSuffix + (valueNegative && sign === "(" ? ")" : "");
          if (maybeSuffix) {
            i = -1, n = value.length;
            while (++i < n) {
              if (c3 = value.charCodeAt(i), 48 > c3 || c3 > 57) {
                valueSuffix = (c3 === 46 ? decimal + value.slice(i + 1) : value.slice(i)) + valueSuffix;
                value = value.slice(0, i);
                break;
              }
            }
          }
        }
        if (comma && !zero3)
          value = group(value, Infinity);
        var length = valuePrefix.length + value.length + valueSuffix.length, padding = length < width ? new Array(width - length + 1).join(fill) : "";
        if (comma && zero3)
          value = group(padding + value, padding.length ? width - valueSuffix.length : Infinity), padding = "";
        switch (align) {
          case "<":
            value = valuePrefix + value + valueSuffix + padding;
            break;
          case "=":
            value = valuePrefix + padding + value + valueSuffix;
            break;
          case "^":
            value = padding.slice(0, length = padding.length >> 1) + valuePrefix + value + valueSuffix + padding.slice(length);
            break;
          default:
            value = padding + valuePrefix + value + valueSuffix;
            break;
        }
        return numerals(value);
      }
      format2.toString = function() {
        return specifier + "";
      };
      return format2;
    }
    function formatPrefix2(specifier, value) {
      var f = newFormat((specifier = formatSpecifier(specifier), specifier.type = "f", specifier)), e = Math.max(-8, Math.min(8, Math.floor(exponent_default(value) / 3))) * 3, k = Math.pow(10, -e), prefix = prefixes[8 + e / 3];
      return function(value2) {
        return f(k * value2) + prefix;
      };
    }
    return {
      format: newFormat,
      formatPrefix: formatPrefix2
    };
  }

  // node_modules/d3-format/src/defaultLocale.js
  var locale;
  var format;
  var formatPrefix;
  defaultLocale({
    thousands: ",",
    grouping: [3],
    currency: ["$", ""]
  });
  function defaultLocale(definition) {
    locale = locale_default(definition);
    format = locale.format;
    formatPrefix = locale.formatPrefix;
    return locale;
  }

  // node_modules/d3-format/src/precisionFixed.js
  function precisionFixed_default(step) {
    return Math.max(0, -exponent_default(Math.abs(step)));
  }

  // node_modules/d3-format/src/precisionPrefix.js
  function precisionPrefix_default(step, value) {
    return Math.max(0, Math.max(-8, Math.min(8, Math.floor(exponent_default(value) / 3))) * 3 - exponent_default(Math.abs(step)));
  }

  // node_modules/d3-format/src/precisionRound.js
  function precisionRound_default(step, max3) {
    step = Math.abs(step), max3 = Math.abs(max3) - step;
    return Math.max(0, exponent_default(max3) - exponent_default(step)) + 1;
  }

  // node_modules/d3-hierarchy/src/hierarchy/count.js
  function count2(node) {
    var sum = 0, children2 = node.children, i = children2 && children2.length;
    if (!i)
      sum = 1;
    else
      while (--i >= 0)
        sum += children2[i].value;
    node.value = sum;
  }
  function count_default() {
    return this.eachAfter(count2);
  }

  // node_modules/d3-hierarchy/src/hierarchy/each.js
  function each_default2(callback, that) {
    let index = -1;
    for (const node of this) {
      callback.call(that, node, ++index, this);
    }
    return this;
  }

  // node_modules/d3-hierarchy/src/hierarchy/eachBefore.js
  function eachBefore_default(callback, that) {
    var node = this, nodes = [node], children2, i, index = -1;
    while (node = nodes.pop()) {
      callback.call(that, node, ++index, this);
      if (children2 = node.children) {
        for (i = children2.length - 1; i >= 0; --i) {
          nodes.push(children2[i]);
        }
      }
    }
    return this;
  }

  // node_modules/d3-hierarchy/src/hierarchy/eachAfter.js
  function eachAfter_default(callback, that) {
    var node = this, nodes = [node], next = [], children2, i, n, index = -1;
    while (node = nodes.pop()) {
      next.push(node);
      if (children2 = node.children) {
        for (i = 0, n = children2.length; i < n; ++i) {
          nodes.push(children2[i]);
        }
      }
    }
    while (node = next.pop()) {
      callback.call(that, node, ++index, this);
    }
    return this;
  }

  // node_modules/d3-hierarchy/src/hierarchy/find.js
  function find_default(callback, that) {
    let index = -1;
    for (const node of this) {
      if (callback.call(that, node, ++index, this)) {
        return node;
      }
    }
  }

  // node_modules/d3-hierarchy/src/hierarchy/sum.js
  function sum_default(value) {
    return this.eachAfter(function(node) {
      var sum = +value(node.data) || 0, children2 = node.children, i = children2 && children2.length;
      while (--i >= 0)
        sum += children2[i].value;
      node.value = sum;
    });
  }

  // node_modules/d3-hierarchy/src/hierarchy/sort.js
  function sort_default2(compare) {
    return this.eachBefore(function(node) {
      if (node.children) {
        node.children.sort(compare);
      }
    });
  }

  // node_modules/d3-hierarchy/src/hierarchy/path.js
  function path_default(end) {
    var start2 = this, ancestor = leastCommonAncestor(start2, end), nodes = [start2];
    while (start2 !== ancestor) {
      start2 = start2.parent;
      nodes.push(start2);
    }
    var k = nodes.length;
    while (end !== ancestor) {
      nodes.splice(k, 0, end);
      end = end.parent;
    }
    return nodes;
  }
  function leastCommonAncestor(a2, b) {
    if (a2 === b)
      return a2;
    var aNodes = a2.ancestors(), bNodes = b.ancestors(), c3 = null;
    a2 = aNodes.pop();
    b = bNodes.pop();
    while (a2 === b) {
      c3 = a2;
      a2 = aNodes.pop();
      b = bNodes.pop();
    }
    return c3;
  }

  // node_modules/d3-hierarchy/src/hierarchy/ancestors.js
  function ancestors_default() {
    var node = this, nodes = [node];
    while (node = node.parent) {
      nodes.push(node);
    }
    return nodes;
  }

  // node_modules/d3-hierarchy/src/hierarchy/descendants.js
  function descendants_default() {
    return Array.from(this);
  }

  // node_modules/d3-hierarchy/src/hierarchy/leaves.js
  function leaves_default() {
    var leaves = [];
    this.eachBefore(function(node) {
      if (!node.children) {
        leaves.push(node);
      }
    });
    return leaves;
  }

  // node_modules/d3-hierarchy/src/hierarchy/links.js
  function links_default() {
    var root2 = this, links = [];
    root2.each(function(node) {
      if (node !== root2) {
        links.push({ source: node.parent, target: node });
      }
    });
    return links;
  }

  // node_modules/d3-hierarchy/src/hierarchy/iterator.js
  function* iterator_default2() {
    var node = this, current, next = [node], children2, i, n;
    do {
      current = next.reverse(), next = [];
      while (node = current.pop()) {
        yield node;
        if (children2 = node.children) {
          for (i = 0, n = children2.length; i < n; ++i) {
            next.push(children2[i]);
          }
        }
      }
    } while (next.length);
  }

  // node_modules/d3-hierarchy/src/hierarchy/index.js
  function hierarchy(data, children2) {
    if (data instanceof Map) {
      data = [void 0, data];
      if (children2 === void 0)
        children2 = mapChildren;
    } else if (children2 === void 0) {
      children2 = objectChildren;
    }
    var root2 = new Node(data), node, nodes = [root2], child, childs, i, n;
    while (node = nodes.pop()) {
      if ((childs = children2(node.data)) && (n = (childs = Array.from(childs)).length)) {
        node.children = childs;
        for (i = n - 1; i >= 0; --i) {
          nodes.push(child = childs[i] = new Node(childs[i]));
          child.parent = node;
          child.depth = node.depth + 1;
        }
      }
    }
    return root2.eachBefore(computeHeight);
  }
  function node_copy() {
    return hierarchy(this).eachBefore(copyData);
  }
  function objectChildren(d) {
    return d.children;
  }
  function mapChildren(d) {
    return Array.isArray(d) ? d[1] : null;
  }
  function copyData(node) {
    if (node.data.value !== void 0)
      node.value = node.data.value;
    node.data = node.data.data;
  }
  function computeHeight(node) {
    var height = 0;
    do
      node.height = height;
    while ((node = node.parent) && node.height < ++height);
  }
  function Node(data) {
    this.data = data;
    this.depth = this.height = 0;
    this.parent = null;
  }
  Node.prototype = hierarchy.prototype = {
    constructor: Node,
    count: count_default,
    each: each_default2,
    eachAfter: eachAfter_default,
    eachBefore: eachBefore_default,
    find: find_default,
    sum: sum_default,
    sort: sort_default2,
    path: path_default,
    ancestors: ancestors_default,
    descendants: descendants_default,
    leaves: leaves_default,
    links: links_default,
    copy: node_copy,
    [Symbol.iterator]: iterator_default2
  };

  // node_modules/d3-hierarchy/src/accessors.js
  function optional(f) {
    return f == null ? null : required(f);
  }
  function required(f) {
    if (typeof f !== "function")
      throw new Error();
    return f;
  }

  // node_modules/d3-hierarchy/src/constant.js
  function constantZero() {
    return 0;
  }
  function constant_default4(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-hierarchy/src/lcg.js
  var a = 1664525;
  var c = 1013904223;
  var m = 4294967296;
  function lcg_default() {
    let s = 1;
    return () => (s = (a * s + c) % m) / m;
  }

  // node_modules/d3-hierarchy/src/array.js
  function array_default(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }
  function shuffle(array3, random) {
    let m2 = array3.length, t, i;
    while (m2) {
      i = random() * m2-- | 0;
      t = array3[m2];
      array3[m2] = array3[i];
      array3[i] = t;
    }
    return array3;
  }

  // node_modules/d3-hierarchy/src/pack/enclose.js
  function packEncloseRandom(circles, random) {
    var i = 0, n = (circles = shuffle(Array.from(circles), random)).length, B2 = [], p, e;
    while (i < n) {
      p = circles[i];
      if (e && enclosesWeak(e, p))
        ++i;
      else
        e = encloseBasis(B2 = extendBasis(B2, p)), i = 0;
    }
    return e;
  }
  function extendBasis(B2, p) {
    var i, j;
    if (enclosesWeakAll(p, B2))
      return [p];
    for (i = 0; i < B2.length; ++i) {
      if (enclosesNot(p, B2[i]) && enclosesWeakAll(encloseBasis2(B2[i], p), B2)) {
        return [B2[i], p];
      }
    }
    for (i = 0; i < B2.length - 1; ++i) {
      for (j = i + 1; j < B2.length; ++j) {
        if (enclosesNot(encloseBasis2(B2[i], B2[j]), p) && enclosesNot(encloseBasis2(B2[i], p), B2[j]) && enclosesNot(encloseBasis2(B2[j], p), B2[i]) && enclosesWeakAll(encloseBasis3(B2[i], B2[j], p), B2)) {
          return [B2[i], B2[j], p];
        }
      }
    }
    throw new Error();
  }
  function enclosesNot(a2, b) {
    var dr = a2.r - b.r, dx = b.x - a2.x, dy = b.y - a2.y;
    return dr < 0 || dr * dr < dx * dx + dy * dy;
  }
  function enclosesWeak(a2, b) {
    var dr = a2.r - b.r + Math.max(a2.r, b.r, 1) * 1e-9, dx = b.x - a2.x, dy = b.y - a2.y;
    return dr > 0 && dr * dr > dx * dx + dy * dy;
  }
  function enclosesWeakAll(a2, B2) {
    for (var i = 0; i < B2.length; ++i) {
      if (!enclosesWeak(a2, B2[i])) {
        return false;
      }
    }
    return true;
  }
  function encloseBasis(B2) {
    switch (B2.length) {
      case 1:
        return encloseBasis1(B2[0]);
      case 2:
        return encloseBasis2(B2[0], B2[1]);
      case 3:
        return encloseBasis3(B2[0], B2[1], B2[2]);
    }
  }
  function encloseBasis1(a2) {
    return {
      x: a2.x,
      y: a2.y,
      r: a2.r
    };
  }
  function encloseBasis2(a2, b) {
    var x1 = a2.x, y1 = a2.y, r1 = a2.r, x2 = b.x, y2 = b.y, r2 = b.r, x21 = x2 - x1, y21 = y2 - y1, r21 = r2 - r1, l = Math.sqrt(x21 * x21 + y21 * y21);
    return {
      x: (x1 + x2 + x21 / l * r21) / 2,
      y: (y1 + y2 + y21 / l * r21) / 2,
      r: (l + r1 + r2) / 2
    };
  }
  function encloseBasis3(a2, b, c3) {
    var x1 = a2.x, y1 = a2.y, r1 = a2.r, x2 = b.x, y2 = b.y, r2 = b.r, x3 = c3.x, y3 = c3.y, r3 = c3.r, a22 = x1 - x2, a3 = x1 - x3, b2 = y1 - y2, b3 = y1 - y3, c22 = r2 - r1, c32 = r3 - r1, d1 = x1 * x1 + y1 * y1 - r1 * r1, d2 = d1 - x2 * x2 - y2 * y2 + r2 * r2, d3 = d1 - x3 * x3 - y3 * y3 + r3 * r3, ab = a3 * b2 - a22 * b3, xa = (b2 * d3 - b3 * d2) / (ab * 2) - x1, xb = (b3 * c22 - b2 * c32) / ab, ya = (a3 * d2 - a22 * d3) / (ab * 2) - y1, yb = (a22 * c32 - a3 * c22) / ab, A2 = xb * xb + yb * yb - 1, B2 = 2 * (r1 + xa * xb + ya * yb), C2 = xa * xa + ya * ya - r1 * r1, r = -(Math.abs(A2) > 1e-6 ? (B2 + Math.sqrt(B2 * B2 - 4 * A2 * C2)) / (2 * A2) : C2 / B2);
    return {
      x: x1 + xa + xb * r,
      y: y1 + ya + yb * r,
      r
    };
  }

  // node_modules/d3-hierarchy/src/pack/siblings.js
  function place(b, a2, c3) {
    var dx = b.x - a2.x, x2, a22, dy = b.y - a2.y, y2, b2, d2 = dx * dx + dy * dy;
    if (d2) {
      a22 = a2.r + c3.r, a22 *= a22;
      b2 = b.r + c3.r, b2 *= b2;
      if (a22 > b2) {
        x2 = (d2 + b2 - a22) / (2 * d2);
        y2 = Math.sqrt(Math.max(0, b2 / d2 - x2 * x2));
        c3.x = b.x - x2 * dx - y2 * dy;
        c3.y = b.y - x2 * dy + y2 * dx;
      } else {
        x2 = (d2 + a22 - b2) / (2 * d2);
        y2 = Math.sqrt(Math.max(0, a22 / d2 - x2 * x2));
        c3.x = a2.x + x2 * dx - y2 * dy;
        c3.y = a2.y + x2 * dy + y2 * dx;
      }
    } else {
      c3.x = a2.x + c3.r;
      c3.y = a2.y;
    }
  }
  function intersects(a2, b) {
    var dr = a2.r + b.r - 1e-6, dx = b.x - a2.x, dy = b.y - a2.y;
    return dr > 0 && dr * dr > dx * dx + dy * dy;
  }
  function score(node) {
    var a2 = node._, b = node.next._, ab = a2.r + b.r, dx = (a2.x * b.r + b.x * a2.r) / ab, dy = (a2.y * b.r + b.y * a2.r) / ab;
    return dx * dx + dy * dy;
  }
  function Node2(circle) {
    this._ = circle;
    this.next = null;
    this.previous = null;
  }
  function packSiblingsRandom(circles, random) {
    if (!(n = (circles = array_default(circles)).length))
      return 0;
    var a2, b, c3, n, aa, ca, i, j, k, sj, sk;
    a2 = circles[0], a2.x = 0, a2.y = 0;
    if (!(n > 1))
      return a2.r;
    b = circles[1], a2.x = -b.r, b.x = a2.r, b.y = 0;
    if (!(n > 2))
      return a2.r + b.r;
    place(b, a2, c3 = circles[2]);
    a2 = new Node2(a2), b = new Node2(b), c3 = new Node2(c3);
    a2.next = c3.previous = b;
    b.next = a2.previous = c3;
    c3.next = b.previous = a2;
    pack:
      for (i = 3; i < n; ++i) {
        place(a2._, b._, c3 = circles[i]), c3 = new Node2(c3);
        j = b.next, k = a2.previous, sj = b._.r, sk = a2._.r;
        do {
          if (sj <= sk) {
            if (intersects(j._, c3._)) {
              b = j, a2.next = b, b.previous = a2, --i;
              continue pack;
            }
            sj += j._.r, j = j.next;
          } else {
            if (intersects(k._, c3._)) {
              a2 = k, a2.next = b, b.previous = a2, --i;
              continue pack;
            }
            sk += k._.r, k = k.previous;
          }
        } while (j !== k.next);
        c3.previous = a2, c3.next = b, a2.next = b.previous = b = c3;
        aa = score(a2);
        while ((c3 = c3.next) !== b) {
          if ((ca = score(c3)) < aa) {
            a2 = c3, aa = ca;
          }
        }
        b = a2.next;
      }
    a2 = [b._], c3 = b;
    while ((c3 = c3.next) !== b)
      a2.push(c3._);
    c3 = packEncloseRandom(a2, random);
    for (i = 0; i < n; ++i)
      a2 = circles[i], a2.x -= c3.x, a2.y -= c3.y;
    return c3.r;
  }

  // node_modules/d3-hierarchy/src/pack/index.js
  function defaultRadius(d) {
    return Math.sqrt(d.value);
  }
  function pack_default() {
    var radius = null, dx = 1, dy = 1, padding = constantZero;
    function pack(root2) {
      const random = lcg_default();
      root2.x = dx / 2, root2.y = dy / 2;
      if (radius) {
        root2.eachBefore(radiusLeaf(radius)).eachAfter(packChildrenRandom(padding, 0.5, random)).eachBefore(translateChild(1));
      } else {
        root2.eachBefore(radiusLeaf(defaultRadius)).eachAfter(packChildrenRandom(constantZero, 1, random)).eachAfter(packChildrenRandom(padding, root2.r / Math.min(dx, dy), random)).eachBefore(translateChild(Math.min(dx, dy) / (2 * root2.r)));
      }
      return root2;
    }
    pack.radius = function(x2) {
      return arguments.length ? (radius = optional(x2), pack) : radius;
    };
    pack.size = function(x2) {
      return arguments.length ? (dx = +x2[0], dy = +x2[1], pack) : [dx, dy];
    };
    pack.padding = function(x2) {
      return arguments.length ? (padding = typeof x2 === "function" ? x2 : constant_default4(+x2), pack) : padding;
    };
    return pack;
  }
  function radiusLeaf(radius) {
    return function(node) {
      if (!node.children) {
        node.r = Math.max(0, +radius(node) || 0);
      }
    };
  }
  function packChildrenRandom(padding, k, random) {
    return function(node) {
      if (children2 = node.children) {
        var children2, i, n = children2.length, r = padding(node) * k || 0, e;
        if (r)
          for (i = 0; i < n; ++i)
            children2[i].r += r;
        e = packSiblingsRandom(children2, random);
        if (r)
          for (i = 0; i < n; ++i)
            children2[i].r -= r;
        node.r = e + r;
      }
    };
  }
  function translateChild(k) {
    return function(node) {
      var parent = node.parent;
      node.r *= k;
      if (parent) {
        node.x = parent.x + k * node.x;
        node.y = parent.y + k * node.y;
      }
    };
  }

  // node_modules/d3-scale/src/init.js
  function initRange(domain, range2) {
    switch (arguments.length) {
      case 0:
        break;
      case 1:
        this.range(domain);
        break;
      default:
        this.range(range2).domain(domain);
        break;
    }
    return this;
  }
  function initInterpolator(domain, interpolator) {
    switch (arguments.length) {
      case 0:
        break;
      case 1: {
        if (typeof domain === "function")
          this.interpolator(domain);
        else
          this.range(domain);
        break;
      }
      default: {
        this.domain(domain);
        if (typeof interpolator === "function")
          this.interpolator(interpolator);
        else
          this.range(interpolator);
        break;
      }
    }
    return this;
  }

  // node_modules/d3-scale/src/ordinal.js
  var implicit = Symbol("implicit");
  function ordinal() {
    var index = new InternMap(), domain = [], range2 = [], unknown = implicit;
    function scale(d) {
      let i = index.get(d);
      if (i === void 0) {
        if (unknown !== implicit)
          return unknown;
        index.set(d, i = domain.push(d) - 1);
      }
      return range2[i % range2.length];
    }
    scale.domain = function(_) {
      if (!arguments.length)
        return domain.slice();
      domain = [], index = new InternMap();
      for (const value of _) {
        if (index.has(value))
          continue;
        index.set(value, domain.push(value) - 1);
      }
      return scale;
    };
    scale.range = function(_) {
      return arguments.length ? (range2 = Array.from(_), scale) : range2.slice();
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    scale.copy = function() {
      return ordinal(domain, range2).unknown(unknown);
    };
    initRange.apply(scale, arguments);
    return scale;
  }

  // node_modules/d3-scale/src/band.js
  function band() {
    var scale = ordinal().unknown(void 0), domain = scale.domain, ordinalRange = scale.range, r0 = 0, r1 = 1, step, bandwidth, round = false, paddingInner = 0, paddingOuter = 0, align = 0.5;
    delete scale.unknown;
    function rescale() {
      var n = domain().length, reverse = r1 < r0, start2 = reverse ? r1 : r0, stop = reverse ? r0 : r1;
      step = (stop - start2) / Math.max(1, n - paddingInner + paddingOuter * 2);
      if (round)
        step = Math.floor(step);
      start2 += (stop - start2 - step * (n - paddingInner)) * align;
      bandwidth = step * (1 - paddingInner);
      if (round)
        start2 = Math.round(start2), bandwidth = Math.round(bandwidth);
      var values = range(n).map(function(i) {
        return start2 + step * i;
      });
      return ordinalRange(reverse ? values.reverse() : values);
    }
    scale.domain = function(_) {
      return arguments.length ? (domain(_), rescale()) : domain();
    };
    scale.range = function(_) {
      return arguments.length ? ([r0, r1] = _, r0 = +r0, r1 = +r1, rescale()) : [r0, r1];
    };
    scale.rangeRound = function(_) {
      return [r0, r1] = _, r0 = +r0, r1 = +r1, round = true, rescale();
    };
    scale.bandwidth = function() {
      return bandwidth;
    };
    scale.step = function() {
      return step;
    };
    scale.round = function(_) {
      return arguments.length ? (round = !!_, rescale()) : round;
    };
    scale.padding = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, paddingOuter = +_), rescale()) : paddingInner;
    };
    scale.paddingInner = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, _), rescale()) : paddingInner;
    };
    scale.paddingOuter = function(_) {
      return arguments.length ? (paddingOuter = +_, rescale()) : paddingOuter;
    };
    scale.align = function(_) {
      return arguments.length ? (align = Math.max(0, Math.min(1, _)), rescale()) : align;
    };
    scale.copy = function() {
      return band(domain(), [r0, r1]).round(round).paddingInner(paddingInner).paddingOuter(paddingOuter).align(align);
    };
    return initRange.apply(rescale(), arguments);
  }
  function pointish(scale) {
    var copy3 = scale.copy;
    scale.padding = scale.paddingOuter;
    delete scale.paddingInner;
    delete scale.paddingOuter;
    scale.copy = function() {
      return pointish(copy3());
    };
    return scale;
  }
  function point() {
    return pointish(band.apply(null, arguments).paddingInner(1));
  }

  // node_modules/d3-scale/src/constant.js
  function constants(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-scale/src/number.js
  function number3(x2) {
    return +x2;
  }

  // node_modules/d3-scale/src/continuous.js
  var unit = [0, 1];
  function identity3(x2) {
    return x2;
  }
  function normalize(a2, b) {
    return (b -= a2 = +a2) ? function(x2) {
      return (x2 - a2) / b;
    } : constants(isNaN(b) ? NaN : 0.5);
  }
  function clamper(a2, b) {
    var t;
    if (a2 > b)
      t = a2, a2 = b, b = t;
    return function(x2) {
      return Math.max(a2, Math.min(b, x2));
    };
  }
  function bimap(domain, range2, interpolate) {
    var d0 = domain[0], d1 = domain[1], r0 = range2[0], r1 = range2[1];
    if (d1 < d0)
      d0 = normalize(d1, d0), r0 = interpolate(r1, r0);
    else
      d0 = normalize(d0, d1), r0 = interpolate(r0, r1);
    return function(x2) {
      return r0(d0(x2));
    };
  }
  function polymap(domain, range2, interpolate) {
    var j = Math.min(domain.length, range2.length) - 1, d = new Array(j), r = new Array(j), i = -1;
    if (domain[j] < domain[0]) {
      domain = domain.slice().reverse();
      range2 = range2.slice().reverse();
    }
    while (++i < j) {
      d[i] = normalize(domain[i], domain[i + 1]);
      r[i] = interpolate(range2[i], range2[i + 1]);
    }
    return function(x2) {
      var i2 = bisect_default(domain, x2, 1, j) - 1;
      return r[i2](d[i2](x2));
    };
  }
  function copy(source, target) {
    return target.domain(source.domain()).range(source.range()).interpolate(source.interpolate()).clamp(source.clamp()).unknown(source.unknown());
  }
  function transformer() {
    var domain = unit, range2 = unit, interpolate = value_default, transform2, untransform, unknown, clamp = identity3, piecewise, output2, input;
    function rescale() {
      var n = Math.min(domain.length, range2.length);
      if (clamp !== identity3)
        clamp = clamper(domain[0], domain[n - 1]);
      piecewise = n > 2 ? polymap : bimap;
      output2 = input = null;
      return scale;
    }
    function scale(x2) {
      return x2 == null || isNaN(x2 = +x2) ? unknown : (output2 || (output2 = piecewise(domain.map(transform2), range2, interpolate)))(transform2(clamp(x2)));
    }
    scale.invert = function(y2) {
      return clamp(untransform((input || (input = piecewise(range2, domain.map(transform2), number_default)))(y2)));
    };
    scale.domain = function(_) {
      return arguments.length ? (domain = Array.from(_, number3), rescale()) : domain.slice();
    };
    scale.range = function(_) {
      return arguments.length ? (range2 = Array.from(_), rescale()) : range2.slice();
    };
    scale.rangeRound = function(_) {
      return range2 = Array.from(_), interpolate = round_default, rescale();
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = _ ? true : identity3, rescale()) : clamp !== identity3;
    };
    scale.interpolate = function(_) {
      return arguments.length ? (interpolate = _, rescale()) : interpolate;
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t, u) {
      transform2 = t, untransform = u;
      return rescale();
    };
  }
  function continuous() {
    return transformer()(identity3, identity3);
  }

  // node_modules/d3-scale/src/tickFormat.js
  function tickFormat(start2, stop, count3, specifier) {
    var step = tickStep(start2, stop, count3), precision;
    specifier = formatSpecifier(specifier == null ? ",f" : specifier);
    switch (specifier.type) {
      case "s": {
        var value = Math.max(Math.abs(start2), Math.abs(stop));
        if (specifier.precision == null && !isNaN(precision = precisionPrefix_default(step, value)))
          specifier.precision = precision;
        return formatPrefix(specifier, value);
      }
      case "":
      case "e":
      case "g":
      case "p":
      case "r": {
        if (specifier.precision == null && !isNaN(precision = precisionRound_default(step, Math.max(Math.abs(start2), Math.abs(stop)))))
          specifier.precision = precision - (specifier.type === "e");
        break;
      }
      case "f":
      case "%": {
        if (specifier.precision == null && !isNaN(precision = precisionFixed_default(step)))
          specifier.precision = precision - (specifier.type === "%") * 2;
        break;
      }
    }
    return format(specifier);
  }

  // node_modules/d3-scale/src/linear.js
  function linearish(scale) {
    var domain = scale.domain;
    scale.ticks = function(count3) {
      var d = domain();
      return ticks(d[0], d[d.length - 1], count3 == null ? 10 : count3);
    };
    scale.tickFormat = function(count3, specifier) {
      var d = domain();
      return tickFormat(d[0], d[d.length - 1], count3 == null ? 10 : count3, specifier);
    };
    scale.nice = function(count3) {
      if (count3 == null)
        count3 = 10;
      var d = domain();
      var i0 = 0;
      var i1 = d.length - 1;
      var start2 = d[i0];
      var stop = d[i1];
      var prestep;
      var step;
      var maxIter = 10;
      if (stop < start2) {
        step = start2, start2 = stop, stop = step;
        step = i0, i0 = i1, i1 = step;
      }
      while (maxIter-- > 0) {
        step = tickIncrement(start2, stop, count3);
        if (step === prestep) {
          d[i0] = start2;
          d[i1] = stop;
          return domain(d);
        } else if (step > 0) {
          start2 = Math.floor(start2 / step) * step;
          stop = Math.ceil(stop / step) * step;
        } else if (step < 0) {
          start2 = Math.ceil(start2 * step) / step;
          stop = Math.floor(stop * step) / step;
        } else {
          break;
        }
        prestep = step;
      }
      return scale;
    };
    return scale;
  }
  function linear2() {
    var scale = continuous();
    scale.copy = function() {
      return copy(scale, linear2());
    };
    initRange.apply(scale, arguments);
    return linearish(scale);
  }

  // node_modules/d3-scale/src/quantize.js
  function quantize() {
    var x0 = 0, x1 = 1, n = 1, domain = [0.5], range2 = [0, 1], unknown;
    function scale(x2) {
      return x2 != null && x2 <= x2 ? range2[bisect_default(domain, x2, 0, n)] : unknown;
    }
    function rescale() {
      var i = -1;
      domain = new Array(n);
      while (++i < n)
        domain[i] = ((i + 1) * x1 - (i - n) * x0) / (n + 1);
      return scale;
    }
    scale.domain = function(_) {
      return arguments.length ? ([x0, x1] = _, x0 = +x0, x1 = +x1, rescale()) : [x0, x1];
    };
    scale.range = function(_) {
      return arguments.length ? (n = (range2 = Array.from(_)).length - 1, rescale()) : range2.slice();
    };
    scale.invertExtent = function(y2) {
      var i = range2.indexOf(y2);
      return i < 0 ? [NaN, NaN] : i < 1 ? [x0, domain[0]] : i >= n ? [domain[n - 1], x1] : [domain[i - 1], domain[i]];
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : scale;
    };
    scale.thresholds = function() {
      return domain.slice();
    };
    scale.copy = function() {
      return quantize().domain([x0, x1]).range(range2).unknown(unknown);
    };
    return initRange.apply(linearish(scale), arguments);
  }

  // node_modules/d3-scale/src/sequential.js
  function transformer2() {
    var x0 = 0, x1 = 1, t0, t1, k10, transform2, interpolator = identity3, clamp = false, unknown;
    function scale(x2) {
      return x2 == null || isNaN(x2 = +x2) ? unknown : interpolator(k10 === 0 ? 0.5 : (x2 = (transform2(x2) - t0) * k10, clamp ? Math.max(0, Math.min(1, x2)) : x2));
    }
    scale.domain = function(_) {
      return arguments.length ? ([x0, x1] = _, t0 = transform2(x0 = +x0), t1 = transform2(x1 = +x1), k10 = t0 === t1 ? 0 : 1 / (t1 - t0), scale) : [x0, x1];
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = !!_, scale) : clamp;
    };
    scale.interpolator = function(_) {
      return arguments.length ? (interpolator = _, scale) : interpolator;
    };
    function range2(interpolate) {
      return function(_) {
        var r0, r1;
        return arguments.length ? ([r0, r1] = _, interpolator = interpolate(r0, r1), scale) : [interpolator(0), interpolator(1)];
      };
    }
    scale.range = range2(value_default);
    scale.rangeRound = range2(round_default);
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t) {
      transform2 = t, t0 = t(x0), t1 = t(x1), k10 = t0 === t1 ? 0 : 1 / (t1 - t0);
      return scale;
    };
  }
  function copy2(source, target) {
    return target.domain(source.domain()).interpolator(source.interpolator()).clamp(source.clamp()).unknown(source.unknown());
  }
  function sequential() {
    var scale = linearish(transformer2()(identity3));
    scale.copy = function() {
      return copy2(scale, sequential());
    };
    return initInterpolator.apply(scale, arguments);
  }

  // node_modules/d3-scale-chromatic/src/colors.js
  function colors_default(specifier) {
    var n = specifier.length / 6 | 0, colors = new Array(n), i = 0;
    while (i < n)
      colors[i] = "#" + specifier.slice(i * 6, ++i * 6);
    return colors;
  }

  // node_modules/d3-scale-chromatic/src/categorical/category10.js
  var category10_default = colors_default("1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf");

  // node_modules/d3-scale-chromatic/src/categorical/Accent.js
  var Accent_default = colors_default("7fc97fbeaed4fdc086ffff99386cb0f0027fbf5b17666666");

  // node_modules/d3-scale-chromatic/src/categorical/Dark2.js
  var Dark2_default = colors_default("1b9e77d95f027570b3e7298a66a61ee6ab02a6761d666666");

  // node_modules/d3-scale-chromatic/src/categorical/Paired.js
  var Paired_default = colors_default("a6cee31f78b4b2df8a33a02cfb9a99e31a1cfdbf6fff7f00cab2d66a3d9affff99b15928");

  // node_modules/d3-scale-chromatic/src/categorical/Set1.js
  var Set1_default = colors_default("e41a1c377eb84daf4a984ea3ff7f00ffff33a65628f781bf999999");

  // node_modules/d3-scale-chromatic/src/ramp.js
  var ramp_default = (scheme20) => rgbBasis(scheme20[scheme20.length - 1]);

  // node_modules/d3-scale-chromatic/src/diverging/RdBu.js
  var scheme = new Array(3).concat(
    "ef8a62f7f7f767a9cf",
    "ca0020f4a58292c5de0571b0",
    "ca0020f4a582f7f7f792c5de0571b0",
    "b2182bef8a62fddbc7d1e5f067a9cf2166ac",
    "b2182bef8a62fddbc7f7f7f7d1e5f067a9cf2166ac",
    "b2182bd6604df4a582fddbc7d1e5f092c5de4393c32166ac",
    "b2182bd6604df4a582fddbc7f7f7f7d1e5f092c5de4393c32166ac",
    "67001fb2182bd6604df4a582fddbc7d1e5f092c5de4393c32166ac053061",
    "67001fb2182bd6604df4a582fddbc7f7f7f7d1e5f092c5de4393c32166ac053061"
  ).map(colors_default);
  var RdBu_default = ramp_default(scheme);

  // node_modules/d3-scale-chromatic/src/sequential-multi/BuGn.js
  var scheme2 = new Array(3).concat(
    "e5f5f999d8c92ca25f",
    "edf8fbb2e2e266c2a4238b45",
    "edf8fbb2e2e266c2a42ca25f006d2c",
    "edf8fbccece699d8c966c2a42ca25f006d2c",
    "edf8fbccece699d8c966c2a441ae76238b45005824",
    "f7fcfde5f5f9ccece699d8c966c2a441ae76238b45005824",
    "f7fcfde5f5f9ccece699d8c966c2a441ae76238b45006d2c00441b"
  ).map(colors_default);
  var BuGn_default = ramp_default(scheme2);

  // node_modules/d3-scale-chromatic/src/sequential-multi/BuPu.js
  var scheme3 = new Array(3).concat(
    "e0ecf49ebcda8856a7",
    "edf8fbb3cde38c96c688419d",
    "edf8fbb3cde38c96c68856a7810f7c",
    "edf8fbbfd3e69ebcda8c96c68856a7810f7c",
    "edf8fbbfd3e69ebcda8c96c68c6bb188419d6e016b",
    "f7fcfde0ecf4bfd3e69ebcda8c96c68c6bb188419d6e016b",
    "f7fcfde0ecf4bfd3e69ebcda8c96c68c6bb188419d810f7c4d004b"
  ).map(colors_default);
  var BuPu_default = ramp_default(scheme3);

  // node_modules/d3-scale-chromatic/src/sequential-multi/GnBu.js
  var scheme4 = new Array(3).concat(
    "e0f3dba8ddb543a2ca",
    "f0f9e8bae4bc7bccc42b8cbe",
    "f0f9e8bae4bc7bccc443a2ca0868ac",
    "f0f9e8ccebc5a8ddb57bccc443a2ca0868ac",
    "f0f9e8ccebc5a8ddb57bccc44eb3d32b8cbe08589e",
    "f7fcf0e0f3dbccebc5a8ddb57bccc44eb3d32b8cbe08589e",
    "f7fcf0e0f3dbccebc5a8ddb57bccc44eb3d32b8cbe0868ac084081"
  ).map(colors_default);
  var GnBu_default = ramp_default(scheme4);

  // node_modules/d3-scale-chromatic/src/sequential-multi/OrRd.js
  var scheme5 = new Array(3).concat(
    "fee8c8fdbb84e34a33",
    "fef0d9fdcc8afc8d59d7301f",
    "fef0d9fdcc8afc8d59e34a33b30000",
    "fef0d9fdd49efdbb84fc8d59e34a33b30000",
    "fef0d9fdd49efdbb84fc8d59ef6548d7301f990000",
    "fff7ecfee8c8fdd49efdbb84fc8d59ef6548d7301f990000",
    "fff7ecfee8c8fdd49efdbb84fc8d59ef6548d7301fb300007f0000"
  ).map(colors_default);
  var OrRd_default = ramp_default(scheme5);

  // node_modules/d3-scale-chromatic/src/sequential-multi/PuBuGn.js
  var scheme6 = new Array(3).concat(
    "ece2f0a6bddb1c9099",
    "f6eff7bdc9e167a9cf02818a",
    "f6eff7bdc9e167a9cf1c9099016c59",
    "f6eff7d0d1e6a6bddb67a9cf1c9099016c59",
    "f6eff7d0d1e6a6bddb67a9cf3690c002818a016450",
    "fff7fbece2f0d0d1e6a6bddb67a9cf3690c002818a016450",
    "fff7fbece2f0d0d1e6a6bddb67a9cf3690c002818a016c59014636"
  ).map(colors_default);
  var PuBuGn_default = ramp_default(scheme6);

  // node_modules/d3-scale-chromatic/src/sequential-multi/PuBu.js
  var scheme7 = new Array(3).concat(
    "ece7f2a6bddb2b8cbe",
    "f1eef6bdc9e174a9cf0570b0",
    "f1eef6bdc9e174a9cf2b8cbe045a8d",
    "f1eef6d0d1e6a6bddb74a9cf2b8cbe045a8d",
    "f1eef6d0d1e6a6bddb74a9cf3690c00570b0034e7b",
    "fff7fbece7f2d0d1e6a6bddb74a9cf3690c00570b0034e7b",
    "fff7fbece7f2d0d1e6a6bddb74a9cf3690c00570b0045a8d023858"
  ).map(colors_default);
  var PuBu_default = ramp_default(scheme7);

  // node_modules/d3-scale-chromatic/src/sequential-multi/PuRd.js
  var scheme8 = new Array(3).concat(
    "e7e1efc994c7dd1c77",
    "f1eef6d7b5d8df65b0ce1256",
    "f1eef6d7b5d8df65b0dd1c77980043",
    "f1eef6d4b9dac994c7df65b0dd1c77980043",
    "f1eef6d4b9dac994c7df65b0e7298ace125691003f",
    "f7f4f9e7e1efd4b9dac994c7df65b0e7298ace125691003f",
    "f7f4f9e7e1efd4b9dac994c7df65b0e7298ace125698004367001f"
  ).map(colors_default);
  var PuRd_default = ramp_default(scheme8);

  // node_modules/d3-scale-chromatic/src/sequential-multi/RdPu.js
  var scheme9 = new Array(3).concat(
    "fde0ddfa9fb5c51b8a",
    "feebe2fbb4b9f768a1ae017e",
    "feebe2fbb4b9f768a1c51b8a7a0177",
    "feebe2fcc5c0fa9fb5f768a1c51b8a7a0177",
    "feebe2fcc5c0fa9fb5f768a1dd3497ae017e7a0177",
    "fff7f3fde0ddfcc5c0fa9fb5f768a1dd3497ae017e7a0177",
    "fff7f3fde0ddfcc5c0fa9fb5f768a1dd3497ae017e7a017749006a"
  ).map(colors_default);
  var RdPu_default = ramp_default(scheme9);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlGnBu.js
  var scheme10 = new Array(3).concat(
    "edf8b17fcdbb2c7fb8",
    "ffffcca1dab441b6c4225ea8",
    "ffffcca1dab441b6c42c7fb8253494",
    "ffffccc7e9b47fcdbb41b6c42c7fb8253494",
    "ffffccc7e9b47fcdbb41b6c41d91c0225ea80c2c84",
    "ffffd9edf8b1c7e9b47fcdbb41b6c41d91c0225ea80c2c84",
    "ffffd9edf8b1c7e9b47fcdbb41b6c41d91c0225ea8253494081d58"
  ).map(colors_default);
  var YlGnBu_default = ramp_default(scheme10);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlGn.js
  var scheme11 = new Array(3).concat(
    "f7fcb9addd8e31a354",
    "ffffccc2e69978c679238443",
    "ffffccc2e69978c67931a354006837",
    "ffffccd9f0a3addd8e78c67931a354006837",
    "ffffccd9f0a3addd8e78c67941ab5d238443005a32",
    "ffffe5f7fcb9d9f0a3addd8e78c67941ab5d238443005a32",
    "ffffe5f7fcb9d9f0a3addd8e78c67941ab5d238443006837004529"
  ).map(colors_default);
  var YlGn_default = ramp_default(scheme11);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlOrBr.js
  var scheme12 = new Array(3).concat(
    "fff7bcfec44fd95f0e",
    "ffffd4fed98efe9929cc4c02",
    "ffffd4fed98efe9929d95f0e993404",
    "ffffd4fee391fec44ffe9929d95f0e993404",
    "ffffd4fee391fec44ffe9929ec7014cc4c028c2d04",
    "ffffe5fff7bcfee391fec44ffe9929ec7014cc4c028c2d04",
    "ffffe5fff7bcfee391fec44ffe9929ec7014cc4c02993404662506"
  ).map(colors_default);
  var YlOrBr_default = ramp_default(scheme12);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlOrRd.js
  var scheme13 = new Array(3).concat(
    "ffeda0feb24cf03b20",
    "ffffb2fecc5cfd8d3ce31a1c",
    "ffffb2fecc5cfd8d3cf03b20bd0026",
    "ffffb2fed976feb24cfd8d3cf03b20bd0026",
    "ffffb2fed976feb24cfd8d3cfc4e2ae31a1cb10026",
    "ffffccffeda0fed976feb24cfd8d3cfc4e2ae31a1cb10026",
    "ffffccffeda0fed976feb24cfd8d3cfc4e2ae31a1cbd0026800026"
  ).map(colors_default);
  var YlOrRd_default = ramp_default(scheme13);

  // node_modules/d3-scale-chromatic/src/sequential-single/Blues.js
  var scheme14 = new Array(3).concat(
    "deebf79ecae13182bd",
    "eff3ffbdd7e76baed62171b5",
    "eff3ffbdd7e76baed63182bd08519c",
    "eff3ffc6dbef9ecae16baed63182bd08519c",
    "eff3ffc6dbef9ecae16baed64292c62171b5084594",
    "f7fbffdeebf7c6dbef9ecae16baed64292c62171b5084594",
    "f7fbffdeebf7c6dbef9ecae16baed64292c62171b508519c08306b"
  ).map(colors_default);
  var Blues_default = ramp_default(scheme14);

  // node_modules/d3-scale-chromatic/src/sequential-single/Greens.js
  var scheme15 = new Array(3).concat(
    "e5f5e0a1d99b31a354",
    "edf8e9bae4b374c476238b45",
    "edf8e9bae4b374c47631a354006d2c",
    "edf8e9c7e9c0a1d99b74c47631a354006d2c",
    "edf8e9c7e9c0a1d99b74c47641ab5d238b45005a32",
    "f7fcf5e5f5e0c7e9c0a1d99b74c47641ab5d238b45005a32",
    "f7fcf5e5f5e0c7e9c0a1d99b74c47641ab5d238b45006d2c00441b"
  ).map(colors_default);
  var Greens_default = ramp_default(scheme15);

  // node_modules/d3-scale-chromatic/src/sequential-single/Greys.js
  var scheme16 = new Array(3).concat(
    "f0f0f0bdbdbd636363",
    "f7f7f7cccccc969696525252",
    "f7f7f7cccccc969696636363252525",
    "f7f7f7d9d9d9bdbdbd969696636363252525",
    "f7f7f7d9d9d9bdbdbd969696737373525252252525",
    "fffffff0f0f0d9d9d9bdbdbd969696737373525252252525",
    "fffffff0f0f0d9d9d9bdbdbd969696737373525252252525000000"
  ).map(colors_default);
  var Greys_default = ramp_default(scheme16);

  // node_modules/d3-scale-chromatic/src/sequential-single/Purples.js
  var scheme17 = new Array(3).concat(
    "efedf5bcbddc756bb1",
    "f2f0f7cbc9e29e9ac86a51a3",
    "f2f0f7cbc9e29e9ac8756bb154278f",
    "f2f0f7dadaebbcbddc9e9ac8756bb154278f",
    "f2f0f7dadaebbcbddc9e9ac8807dba6a51a34a1486",
    "fcfbfdefedf5dadaebbcbddc9e9ac8807dba6a51a34a1486",
    "fcfbfdefedf5dadaebbcbddc9e9ac8807dba6a51a354278f3f007d"
  ).map(colors_default);
  var Purples_default = ramp_default(scheme17);

  // node_modules/d3-scale-chromatic/src/sequential-single/Reds.js
  var scheme18 = new Array(3).concat(
    "fee0d2fc9272de2d26",
    "fee5d9fcae91fb6a4acb181d",
    "fee5d9fcae91fb6a4ade2d26a50f15",
    "fee5d9fcbba1fc9272fb6a4ade2d26a50f15",
    "fee5d9fcbba1fc9272fb6a4aef3b2ccb181d99000d",
    "fff5f0fee0d2fcbba1fc9272fb6a4aef3b2ccb181d99000d",
    "fff5f0fee0d2fcbba1fc9272fb6a4aef3b2ccb181da50f1567000d"
  ).map(colors_default);
  var Reds_default = ramp_default(scheme18);

  // node_modules/d3-scale-chromatic/src/sequential-single/Oranges.js
  var scheme19 = new Array(3).concat(
    "fee6cefdae6be6550d",
    "feeddefdbe85fd8d3cd94701",
    "feeddefdbe85fd8d3ce6550da63603",
    "feeddefdd0a2fdae6bfd8d3ce6550da63603",
    "feeddefdd0a2fdae6bfd8d3cf16913d948018c2d04",
    "fff5ebfee6cefdd0a2fdae6bfd8d3cf16913d948018c2d04",
    "fff5ebfee6cefdd0a2fdae6bfd8d3cf16913d94801a636037f2704"
  ).map(colors_default);
  var Oranges_default = ramp_default(scheme19);

  // node_modules/d3-scale-chromatic/src/sequential-multi/cubehelix.js
  var cubehelix_default2 = cubehelixLong(cubehelix(300, 0.5, 0), cubehelix(-240, 0.5, 1));

  // node_modules/d3-scale-chromatic/src/sequential-multi/rainbow.js
  var warm = cubehelixLong(cubehelix(-100, 0.75, 0.35), cubehelix(80, 1.5, 0.8));
  var cool = cubehelixLong(cubehelix(260, 0.75, 0.35), cubehelix(80, 1.5, 0.8));
  var c2 = cubehelix();
  function rainbow_default(t) {
    if (t < 0 || t > 1)
      t -= Math.floor(t);
    var ts = Math.abs(t - 0.5);
    c2.h = 360 * t - 100;
    c2.s = 1.5 - 1.5 * ts;
    c2.l = 0.8 - 0.9 * ts;
    return c2 + "";
  }

  // node_modules/d3-scale-chromatic/src/sequential-multi/viridis.js
  function ramp(range2) {
    var n = range2.length;
    return function(t) {
      return range2[Math.max(0, Math.min(n - 1, Math.floor(t * n)))];
    };
  }
  var viridis_default = ramp(colors_default("44015444025645045745055946075a46085c460a5d460b5e470d60470e6147106347116447136548146748166848176948186a481a6c481b6d481c6e481d6f481f70482071482173482374482475482576482677482878482979472a7a472c7a472d7b472e7c472f7d46307e46327e46337f463480453581453781453882443983443a83443b84433d84433e85423f854240864241864142874144874045884046883f47883f48893e49893e4a893e4c8a3d4d8a3d4e8a3c4f8a3c508b3b518b3b528b3a538b3a548c39558c39568c38588c38598c375a8c375b8d365c8d365d8d355e8d355f8d34608d34618d33628d33638d32648e32658e31668e31678e31688e30698e306a8e2f6b8e2f6c8e2e6d8e2e6e8e2e6f8e2d708e2d718e2c718e2c728e2c738e2b748e2b758e2a768e2a778e2a788e29798e297a8e297b8e287c8e287d8e277e8e277f8e27808e26818e26828e26828e25838e25848e25858e24868e24878e23888e23898e238a8d228b8d228c8d228d8d218e8d218f8d21908d21918c20928c20928c20938c1f948c1f958b1f968b1f978b1f988b1f998a1f9a8a1e9b8a1e9c891e9d891f9e891f9f881fa0881fa1881fa1871fa28720a38620a48621a58521a68522a78522a88423a98324aa8325ab8225ac8226ad8127ad8128ae8029af7f2ab07f2cb17e2db27d2eb37c2fb47c31b57b32b67a34b67935b77937b87838b9773aba763bbb753dbc743fbc7340bd7242be7144bf7046c06f48c16e4ac16d4cc26c4ec36b50c46a52c56954c56856c66758c7655ac8645cc8635ec96260ca6063cb5f65cb5e67cc5c69cd5b6ccd5a6ece5870cf5773d05675d05477d1537ad1517cd2507fd34e81d34d84d44b86d54989d5488bd6468ed64590d74393d74195d84098d83e9bd93c9dd93ba0da39a2da37a5db36a8db34aadc32addc30b0dd2fb2dd2db5de2bb8de29bade28bddf26c0df25c2df23c5e021c8e020cae11fcde11dd0e11cd2e21bd5e21ad8e219dae319dde318dfe318e2e418e5e419e7e419eae51aece51befe51cf1e51df4e61ef6e620f8e621fbe723fde725"));
  var magma = ramp(colors_default("00000401000501010601010802010902020b02020d03030f03031204041405041606051806051a07061c08071e0907200a08220b09240c09260d0a290e0b2b100b2d110c2f120d31130d34140e36150e38160f3b180f3d19103f1a10421c10441d11471e114920114b21114e22115024125325125527125829115a2a115c2c115f2d11612f116331116533106734106936106b38106c390f6e3b0f703d0f713f0f72400f74420f75440f764510774710784910784a10794c117a4e117b4f127b51127c52137c54137d56147d57157e59157e5a167e5c167f5d177f5f187f601880621980641a80651a80671b80681c816a1c816b1d816d1d816e1e81701f81721f817320817521817621817822817922827b23827c23827e24828025828125818326818426818627818827818928818b29818c29818e2a81902a81912b81932b80942c80962c80982d80992d809b2e7f9c2e7f9e2f7fa02f7fa1307ea3307ea5317ea6317da8327daa337dab337cad347cae347bb0357bb2357bb3367ab5367ab73779b83779ba3878bc3978bd3977bf3a77c03a76c23b75c43c75c53c74c73d73c83e73ca3e72cc3f71cd4071cf4070d0416fd2426fd3436ed5446dd6456cd8456cd9466bdb476adc4869de4968df4a68e04c67e24d66e34e65e44f64e55064e75263e85362e95462ea5661eb5760ec5860ed5a5fee5b5eef5d5ef05f5ef1605df2625df2645cf3655cf4675cf4695cf56b5cf66c5cf66e5cf7705cf7725cf8745cf8765cf9785df9795df97b5dfa7d5efa7f5efa815ffb835ffb8560fb8761fc8961fc8a62fc8c63fc8e64fc9065fd9266fd9467fd9668fd9869fd9a6afd9b6bfe9d6cfe9f6dfea16efea36ffea571fea772fea973feaa74feac76feae77feb078feb27afeb47bfeb67cfeb77efeb97ffebb81febd82febf84fec185fec287fec488fec68afec88cfeca8dfecc8ffecd90fecf92fed194fed395fed597fed799fed89afdda9cfddc9efddea0fde0a1fde2a3fde3a5fde5a7fde7a9fde9aafdebacfcecaefceeb0fcf0b2fcf2b4fcf4b6fcf6b8fcf7b9fcf9bbfcfbbdfcfdbf"));
  var inferno = ramp(colors_default("00000401000501010601010802010a02020c02020e03021004031204031405041706041907051b08051d09061f0a07220b07240c08260d08290e092b10092d110a30120a32140b34150b37160b39180c3c190c3e1b0c411c0c431e0c451f0c48210c4a230c4c240c4f260c51280b53290b552b0b572d0b592f0a5b310a5c320a5e340a5f3609613809623909633b09643d09653e0966400a67420a68440a68450a69470b6a490b6a4a0c6b4c0c6b4d0d6c4f0d6c510e6c520e6d540f6d550f6d57106e59106e5a116e5c126e5d126e5f136e61136e62146e64156e65156e67166e69166e6a176e6c186e6d186e6f196e71196e721a6e741a6e751b6e771c6d781c6d7a1d6d7c1d6d7d1e6d7f1e6c801f6c82206c84206b85216b87216b88226a8a226a8c23698d23698f24699025689225689326679526679727669827669a28659b29649d29649f2a63a02a63a22b62a32c61a52c60a62d60a82e5fa92e5eab2f5ead305dae305cb0315bb1325ab3325ab43359b63458b73557b93556ba3655bc3754bd3853bf3952c03a51c13a50c33b4fc43c4ec63d4dc73e4cc83f4bca404acb4149cc4248ce4347cf4446d04545d24644d34743d44842d54a41d74b3fd84c3ed94d3dda4e3cdb503bdd513ade5238df5337e05536e15635e25734e35933e45a31e55c30e65d2fe75e2ee8602de9612bea632aeb6429eb6628ec6726ed6925ee6a24ef6c23ef6e21f06f20f1711ff1731df2741cf3761bf37819f47918f57b17f57d15f67e14f68013f78212f78410f8850ff8870ef8890cf98b0bf98c0af98e09fa9008fa9207fa9407fb9606fb9706fb9906fb9b06fb9d07fc9f07fca108fca309fca50afca60cfca80dfcaa0ffcac11fcae12fcb014fcb216fcb418fbb61afbb81dfbba1ffbbc21fbbe23fac026fac228fac42afac62df9c72ff9c932f9cb35f8cd37f8cf3af7d13df7d340f6d543f6d746f5d949f5db4cf4dd4ff4df53f4e156f3e35af3e55df2e661f2e865f2ea69f1ec6df1ed71f1ef75f1f179f2f27df2f482f3f586f3f68af4f88ef5f992f6fa96f8fb9af9fc9dfafda1fcffa4"));
  var plasma = ramp(colors_default("0d088710078813078916078a19068c1b068d1d068e20068f2206902406912605912805922a05932c05942e05952f059631059733059735049837049938049a3a049a3c049b3e049c3f049c41049d43039e44039e46039f48039f4903a04b03a14c02a14e02a25002a25102a35302a35502a45601a45801a45901a55b01a55c01a65e01a66001a66100a76300a76400a76600a76700a86900a86a00a86c00a86e00a86f00a87100a87201a87401a87501a87701a87801a87a02a87b02a87d03a87e03a88004a88104a78305a78405a78606a68707a68808a68a09a58b0aa58d0ba58e0ca48f0da4910ea3920fa39410a29511a19613a19814a099159f9a169f9c179e9d189d9e199da01a9ca11b9ba21d9aa31e9aa51f99a62098a72197a82296aa2395ab2494ac2694ad2793ae2892b02991b12a90b22b8fb32c8eb42e8db52f8cb6308bb7318ab83289ba3388bb3488bc3587bd3786be3885bf3984c03a83c13b82c23c81c33d80c43e7fc5407ec6417dc7427cc8437bc9447aca457acb4679cc4778cc4977cd4a76ce4b75cf4c74d04d73d14e72d24f71d35171d45270d5536fd5546ed6556dd7566cd8576bd9586ada5a6ada5b69db5c68dc5d67dd5e66de5f65de6164df6263e06363e16462e26561e26660e3685fe4695ee56a5de56b5de66c5ce76e5be76f5ae87059e97158e97257ea7457eb7556eb7655ec7754ed7953ed7a52ee7b51ef7c51ef7e50f07f4ff0804ef1814df1834cf2844bf3854bf3874af48849f48948f58b47f58c46f68d45f68f44f79044f79143f79342f89441f89540f9973ff9983ef99a3efa9b3dfa9c3cfa9e3bfb9f3afba139fba238fca338fca537fca636fca835fca934fdab33fdac33fdae32fdaf31fdb130fdb22ffdb42ffdb52efeb72dfeb82cfeba2cfebb2bfebd2afebe2afec029fdc229fdc328fdc527fdc627fdc827fdca26fdcb26fccd25fcce25fcd025fcd225fbd324fbd524fbd724fad824fada24f9dc24f9dd25f8df25f8e125f7e225f7e425f6e626f6e826f5e926f5eb27f4ed27f3ee27f3f027f2f227f1f426f1f525f0f724f0f921"));

  // node_modules/d3-shape/src/constant.js
  function constant_default5(x2) {
    return function constant2() {
      return x2;
    };
  }

  // node_modules/d3-shape/src/path.js
  function withPath(shape) {
    let digits = 3;
    shape.digits = function(_) {
      if (!arguments.length)
        return digits;
      if (_ == null) {
        digits = null;
      } else {
        const d = Math.floor(_);
        if (!(d >= 0))
          throw new RangeError(`invalid digits: ${_}`);
        digits = d;
      }
      return shape;
    };
    return () => new Path(digits);
  }

  // node_modules/d3-shape/src/array.js
  var slice2 = Array.prototype.slice;
  function array_default2(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }

  // node_modules/d3-shape/src/curve/linear.js
  function Linear(context) {
    this._context = context;
  }
  Linear.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._point = 0;
    },
    lineEnd: function() {
      if (this._line || this._line !== 0 && this._point === 1)
        this._context.closePath();
      this._line = 1 - this._line;
    },
    point: function(x2, y2) {
      x2 = +x2, y2 = +y2;
      switch (this._point) {
        case 0:
          this._point = 1;
          this._line ? this._context.lineTo(x2, y2) : this._context.moveTo(x2, y2);
          break;
        case 1:
          this._point = 2;
        default:
          this._context.lineTo(x2, y2);
          break;
      }
    }
  };
  function linear_default(context) {
    return new Linear(context);
  }

  // node_modules/d3-shape/src/point.js
  function x(p) {
    return p[0];
  }
  function y(p) {
    return p[1];
  }

  // node_modules/d3-shape/src/line.js
  function line_default(x2, y2) {
    var defined = constant_default5(true), context = null, curve = linear_default, output2 = null, path2 = withPath(line);
    x2 = typeof x2 === "function" ? x2 : x2 === void 0 ? x : constant_default5(x2);
    y2 = typeof y2 === "function" ? y2 : y2 === void 0 ? y : constant_default5(y2);
    function line(data) {
      var i, n = (data = array_default2(data)).length, d, defined0 = false, buffer;
      if (context == null)
        output2 = curve(buffer = path2());
      for (i = 0; i <= n; ++i) {
        if (!(i < n && defined(d = data[i], i, data)) === defined0) {
          if (defined0 = !defined0)
            output2.lineStart();
          else
            output2.lineEnd();
        }
        if (defined0)
          output2.point(+x2(d, i, data), +y2(d, i, data));
      }
      if (buffer)
        return output2 = null, buffer + "" || null;
    }
    line.x = function(_) {
      return arguments.length ? (x2 = typeof _ === "function" ? _ : constant_default5(+_), line) : x2;
    };
    line.y = function(_) {
      return arguments.length ? (y2 = typeof _ === "function" ? _ : constant_default5(+_), line) : y2;
    };
    line.defined = function(_) {
      return arguments.length ? (defined = typeof _ === "function" ? _ : constant_default5(!!_), line) : defined;
    };
    line.curve = function(_) {
      return arguments.length ? (curve = _, context != null && (output2 = curve(context)), line) : curve;
    };
    line.context = function(_) {
      return arguments.length ? (_ == null ? context = output2 = null : output2 = curve(context = _), line) : context;
    };
    return line;
  }

  // node_modules/d3-shape/src/curve/basis.js
  function point2(that, x2, y2) {
    that._context.bezierCurveTo(
      (2 * that._x0 + that._x1) / 3,
      (2 * that._y0 + that._y1) / 3,
      (that._x0 + 2 * that._x1) / 3,
      (that._y0 + 2 * that._y1) / 3,
      (that._x0 + 4 * that._x1 + x2) / 6,
      (that._y0 + 4 * that._y1 + y2) / 6
    );
  }
  function Basis(context) {
    this._context = context;
  }
  Basis.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._x0 = this._x1 = this._y0 = this._y1 = NaN;
      this._point = 0;
    },
    lineEnd: function() {
      switch (this._point) {
        case 3:
          point2(this, this._x1, this._y1);
        case 2:
          this._context.lineTo(this._x1, this._y1);
          break;
      }
      if (this._line || this._line !== 0 && this._point === 1)
        this._context.closePath();
      this._line = 1 - this._line;
    },
    point: function(x2, y2) {
      x2 = +x2, y2 = +y2;
      switch (this._point) {
        case 0:
          this._point = 1;
          this._line ? this._context.lineTo(x2, y2) : this._context.moveTo(x2, y2);
          break;
        case 1:
          this._point = 2;
          break;
        case 2:
          this._point = 3;
          this._context.lineTo((5 * this._x0 + this._x1) / 6, (5 * this._y0 + this._y1) / 6);
        default:
          point2(this, x2, y2);
          break;
      }
      this._x0 = this._x1, this._x1 = x2;
      this._y0 = this._y1, this._y1 = y2;
    }
  };
  function basis_default2(context) {
    return new Basis(context);
  }

  // node_modules/d3-shape/src/curve/natural.js
  function Natural(context) {
    this._context = context;
  }
  Natural.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._x = [];
      this._y = [];
    },
    lineEnd: function() {
      var x2 = this._x, y2 = this._y, n = x2.length;
      if (n) {
        this._line ? this._context.lineTo(x2[0], y2[0]) : this._context.moveTo(x2[0], y2[0]);
        if (n === 2) {
          this._context.lineTo(x2[1], y2[1]);
        } else {
          var px = controlPoints(x2), py = controlPoints(y2);
          for (var i0 = 0, i1 = 1; i1 < n; ++i0, ++i1) {
            this._context.bezierCurveTo(px[0][i0], py[0][i0], px[1][i0], py[1][i0], x2[i1], y2[i1]);
          }
        }
      }
      if (this._line || this._line !== 0 && n === 1)
        this._context.closePath();
      this._line = 1 - this._line;
      this._x = this._y = null;
    },
    point: function(x2, y2) {
      this._x.push(+x2);
      this._y.push(+y2);
    }
  };
  function controlPoints(x2) {
    var i, n = x2.length - 1, m2, a2 = new Array(n), b = new Array(n), r = new Array(n);
    a2[0] = 0, b[0] = 2, r[0] = x2[0] + 2 * x2[1];
    for (i = 1; i < n - 1; ++i)
      a2[i] = 1, b[i] = 4, r[i] = 4 * x2[i] + 2 * x2[i + 1];
    a2[n - 1] = 2, b[n - 1] = 7, r[n - 1] = 8 * x2[n - 1] + x2[n];
    for (i = 1; i < n; ++i)
      m2 = a2[i] / b[i - 1], b[i] -= m2, r[i] -= m2 * r[i - 1];
    a2[n - 1] = r[n - 1] / b[n - 1];
    for (i = n - 2; i >= 0; --i)
      a2[i] = (r[i] - a2[i + 1]) / b[i];
    b[n - 1] = (x2[n] + a2[n - 1]) / 2;
    for (i = 0; i < n - 1; ++i)
      b[i] = 2 * x2[i + 1] - a2[i + 1];
    return [a2, b];
  }
  function natural_default(context) {
    return new Natural(context);
  }

  // node_modules/d3-zoom/src/transform.js
  function Transform(k, x2, y2) {
    this.k = k;
    this.x = x2;
    this.y = y2;
  }
  Transform.prototype = {
    constructor: Transform,
    scale: function(k) {
      return k === 1 ? this : new Transform(this.k * k, this.x, this.y);
    },
    translate: function(x2, y2) {
      return x2 === 0 & y2 === 0 ? this : new Transform(this.k, this.x + this.k * x2, this.y + this.k * y2);
    },
    apply: function(point3) {
      return [point3[0] * this.k + this.x, point3[1] * this.k + this.y];
    },
    applyX: function(x2) {
      return x2 * this.k + this.x;
    },
    applyY: function(y2) {
      return y2 * this.k + this.y;
    },
    invert: function(location) {
      return [(location[0] - this.x) / this.k, (location[1] - this.y) / this.k];
    },
    invertX: function(x2) {
      return (x2 - this.x) / this.k;
    },
    invertY: function(y2) {
      return (y2 - this.y) / this.k;
    },
    rescaleX: function(x2) {
      return x2.copy().domain(x2.range().map(this.invertX, this).map(x2.invert, x2));
    },
    rescaleY: function(y2) {
      return y2.copy().domain(y2.range().map(this.invertY, this).map(y2.invert, y2));
    },
    toString: function() {
      return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
    }
  };
  var identity4 = new Transform(1, 0, 0);
  transform.prototype = Transform.prototype;
  function transform(node) {
    while (!node.__zoom)
      if (!(node = node.parentNode))
        return identity4;
    return node.__zoom;
  }

  // src/Typescript/brushSlider.ts
  var BrushSlider = class {
    constructor(scatterPlotMatrix, xOriented) {
      this.xOriented = false;
      this.dimIndexScale = point();
      this.dimIndexScaleInvertFn = quantize();
      this.inSelectionDrag = false;
      this.scatterPlotMatrix = scatterPlotMatrix;
      this.xOriented = xOriented;
      this.sliderClass = this.xOriented ? "xSlider" : "ySlider";
      select_default2(scatterPlotMatrix.bindto + " .MultiPlot svg").append("g").attr("class", this.sliderClass);
    }
    update() {
      this.updateDimIndexScale();
      select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .axisGroup`).remove();
      select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .brushDim`).remove();
      if (this.xOriented) {
        this.buildChainGroup();
      }
      const axisGenerator = this.xOriented ? axisBottom(this.dimIndexScale) : axisRight(this.dimIndexScale);
      const axis2 = select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass}`).append("g").attr("pointer-events", "none").attr("class", "axisGroup").call(axisGenerator.tickSize(0).tickFormat(() => ""));
      const dx = this.xOriented ? ScatterPlotMatrix.margin.l : ScatterPlotMatrix.margin.l + ScatterPlotMatrix.margin.r + this.scatterPlotMatrix.getSize() - 16;
      const dy = this.xOriented ? ScatterPlotMatrix.margin.t / 4 : ScatterPlotMatrix.margin.t;
      select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass}`).attr("transform", `translate(${dx}, ${dy})`);
      this.createBrush();
      axis2.append("line").attr("class", "locatorLine").style("display", "none").attr("pointer-events", "none");
      this.adjustBrushSelection();
    }
    buildChainGroup() {
      const thisBS = this;
      select_default2(`${this.scatterPlotMatrix.bindto} .chainGroup`).remove();
      const chainGroup = select_default2(this.scatterPlotMatrix.bindto + " .MultiPlot svg").append("g").attr("class", "chainGroup").attr("transform", `translate(${ScatterPlotMatrix.margin.l + ScatterPlotMatrix.margin.r + this.scatterPlotMatrix.getSize() - 16}, ${ScatterPlotMatrix.margin.t / 4})`).on("mousedown", function() {
        chainGroup.classed("mousedown", true);
        select_default2(window).on("mouseup", function() {
          chainGroup.classed("mousedown", false);
        });
      }).on("click", function() {
        thisBS.scatterPlotMatrix.brushSlidersLinked = !thisBS.scatterPlotMatrix.brushSlidersLinked;
        chainGroup.select("path").attr("transform", `translate(0, ${thisBS.scatterPlotMatrix.brushSlidersLinked ? 0 : -2})`);
        const begin = thisBS.scatterPlotMatrix.xStartingDimIndex;
        const end = thisBS.scatterPlotMatrix.xStartingDimIndex + thisBS.scatterPlotMatrix.visibleDimCount - 1;
        thisBS.scatterPlotMatrix.updateVisibleDimensions(begin, end, thisBS.xOriented);
        thisBS.adjustOtherBrushSelection();
      });
      chainGroup.append("path").attr("d", "M-3,0L-3,-6A3,3 0 0,1 3,-6L3,-3").attr("transform", `translate(0, ${thisBS.scatterPlotMatrix.brushSlidersLinked ? 0 : -2})`);
      chainGroup.append("rect").attr("x", -5).attr("y", -2).attr("width", 10).attr("height", 9);
    }
    updateDimIndexScale() {
      const spData = this.scatterPlotMatrix.spData;
      const size = this.scatterPlotMatrix.getSize();
      this.dimIndexScale.domain(range(spData.dimensions.length)).range([0, size]);
      this.dimIndexScaleInvertFn.domain([0, size]).range(range(spData.dimensions.length));
    }
    centerBrush(indexCenter, moveBrush) {
      const spData = this.scatterPlotMatrix.spData;
      const sizeDimVisible = this.scatterPlotMatrix.visibleDimCount;
      let sizeLeft = Math.round((sizeDimVisible - 1) / 2);
      let sizeRight = sizeDimVisible - 1 - sizeLeft;
      if (indexCenter - sizeLeft < 0) {
        sizeRight = sizeRight + (sizeLeft - indexCenter);
        sizeLeft = indexCenter;
      }
      if (indexCenter + sizeRight > spData.dimensions.length - 1) {
        sizeLeft = sizeLeft + (indexCenter + sizeRight - spData.dimensions.length + 1);
        sizeRight = spData.dimensions.length - 1 - indexCenter;
      }
      const begin = indexCenter - sizeLeft;
      const end = indexCenter + sizeRight;
      if (begin !== this.startingDimIndex() || end !== this.startingDimIndex() + this.scatterPlotMatrix.visibleDimCount - 1) {
        this.scatterPlotMatrix.updateVisibleDimensions(begin, end, this.xOriented);
        select_default2(this.scatterPlotMatrix.bindto + " .mspTooltip").style("display", "none");
      }
      if (moveBrush) {
        this.adjustBrushSelection();
        this.adjustOtherBrushSelection();
      }
    }
    mouseDown(mouse) {
      const dimIndex = this.dimIndexScaleInvertFn(mouse[this.xOriented ? 0 : 1]);
      this.centerBrush(dimIndex, true);
    }
    mouseMove(mouse) {
      const dimIndex = this.dimIndexScaleInvertFn(mouse[this.xOriented ? 0 : 1]);
      if (dimIndex !== void 0) {
        const line = this.xOriented ? [[mouse[0], -4], [mouse[0], 4]] : [[-4, mouse[1]], [4, mouse[1]]];
        select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .locatorLine`).style("display", null).attr("x1", line[0][0]).attr("y1", line[0][1]).attr("x2", line[1][0]).attr("y2", line[1][1]);
        const mspDivNode = select_default2(this.scatterPlotMatrix.bindto + " .mspDiv").node();
        const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
        const xParent = parentBounds === null ? 0 : parentBounds.x;
        const yParent = parentBounds === null ? 0 : parentBounds.y;
        const overlayNode = select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .overlay`).node();
        const overlayBound = overlayNode === null ? null : overlayNode.getBoundingClientRect();
        const xOverlay = overlayBound === null ? 0 : overlayBound.x;
        const yOverlay = overlayBound === null ? 0 : overlayBound.y;
        select_default2(this.scatterPlotMatrix.bindto + " .mspTooltip").remove();
        const mspDiv = select_default2(this.scatterPlotMatrix.bindto + " .mspDiv");
        const dx = this.xOriented ? -xParent + mouse[0] + 15 : -xParent - 15;
        const dy = this.xOriented ? -yParent - 15 : -yParent + mouse[1] + 15;
        const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", xOverlay + dx + "px").style("top", yOverlay + dy + "px");
        tooltip.append("div").html(this.scatterPlotMatrix.spData.columns[this.scatterPlotMatrix.spData.dimensions[dimIndex]].label);
      }
    }
    mouseExit() {
      select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .locatorLine`).style("display", "none");
      select_default2(this.scatterPlotMatrix.bindto + " .mspTooltip").style("display", "none");
    }
    createBrush() {
      this.inSelectionDrag = false;
      select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass}`).append("g").attr("class", "brushDim").call(this.brushBehavior()).call(
        (g) => g.select(".overlay").attr("rx", 5).attr("ry", 5).on("mousedown touchstart", (event) => {
          this.mouseDown(pointer_default(event));
          event.stopPropagation();
        }).on("mousemove", (event) => {
          this.mouseMove(pointer_default(event));
        }).on("mouseout", () => {
          this.mouseExit();
        })
      ).call(
        (g) => g.select(".selection").attr("rx", 5).attr("ry", 5).on("mousedown", () => {
          this.inSelectionDrag = true;
        }).on("mouseup", () => {
          this.inSelectionDrag = false;
        })
      );
    }
    brushBehavior() {
      return this.buildBrushBehavior().handleSize(4).extent(
        this.xOriented ? [
          [0, -5],
          [this.scatterPlotMatrix.getSize(), 5]
        ] : [
          [-5, 0],
          [5, this.scatterPlotMatrix.getSize()]
        ]
      ).on("brush", (event) => {
        const selection2 = event.selection;
        if (this.inSelectionDrag) {
          const brushCenter = (selection2[0] + selection2[1]) / 2;
          const centerIndex = this.dimIndexScaleInvertFn(brushCenter);
          if (centerIndex) {
            this.centerBrush(centerIndex, false);
          }
        } else {
          const begin = this.dimIndexScaleInvertFn(selection2[0]);
          const end = this.dimIndexScaleInvertFn(selection2[1]);
          if (begin !== this.startingDimIndex() || end !== this.startingDimIndex() + this.scatterPlotMatrix.visibleDimCount - 1) {
            this.scatterPlotMatrix.updateVisibleDimensions(begin, end, this.xOriented);
          }
        }
        this.adjustOtherBrushSelection();
      }).on("end", () => {
        this.inSelectionDrag = false;
        this.adjustBrushSelection();
        this.adjustOtherBrushSelection();
      });
    }
    adjustBrushSelection() {
      select_default2(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .brushDim`).call(
        this.buildBrushBehavior().move,
        [
          this.dimIndexScale(this.startingDimIndex()),
          this.dimIndexScale(this.startingDimIndex() + this.scatterPlotMatrix.visibleDimCount - 1)
        ]
      );
    }
    adjustOtherBrushSelection() {
      const otherVisibleDimIndex = this.xOriented ? this.scatterPlotMatrix.yStartingDimIndex : this.scatterPlotMatrix.xStartingDimIndex;
      const otherBrushBehavior = this.xOriented ? brushY() : brushX();
      const otherSlideClass = this.xOriented ? "ySlider" : "xSlider";
      select_default2(`${this.scatterPlotMatrix.bindto} .${otherSlideClass} .brushDim`).call(
        otherBrushBehavior.move,
        [
          this.dimIndexScale(this.scatterPlotMatrix.brushSlidersLinked ? this.startingDimIndex() : otherVisibleDimIndex),
          this.dimIndexScale(otherVisibleDimIndex + this.scatterPlotMatrix.visibleDimCount - 1)
        ]
      );
    }
    // private equals(array1: Array<any>, array2: Array<any>) {
    //     return array1.length === array2.length && array1.every((value, index) => value === array2[index]);
    // }
    startingDimIndex() {
      return this.xOriented ? this.scatterPlotMatrix.xStartingDimIndex : this.scatterPlotMatrix.yStartingDimIndex;
    }
    buildBrushBehavior() {
      return this.xOriented ? brushX() : brushY();
    }
  };

  // src/Typescript/expFormat.ts
  var ExpFormat = class _ExpFormat {
    static {
      this.NONBREAKING_SPACE = String.fromCharCode(160);
    }
    static {
      this.EXP_FORMATS = {
        "y": "-24",
        "z": "-21",
        "a": "-18",
        "f": "-15",
        "p": "-12",
        "n": "-9",
        "\xB5": "-6",
        "m": "-3",
        "k": "3",
        "M": "6",
        "G": "9",
        "T": "12",
        "P": "15",
        "E": "18",
        "Z": "21",
        "Y": "24"
      };
    }
    static {
      this.f2s = format(".2~s");
    }
    static {
      this.f3f = format(".3~r");
    }
    static sToExp(siValue) {
      const siStr = /[yzafpnµmkMGTPEZY]/.exec(siValue);
      if (siStr !== null) {
        return siValue.replace(siStr[0], _ExpFormat.NONBREAKING_SPACE + "E" + _ExpFormat.EXP_FORMATS[siStr[0]]);
      }
      return siValue;
    }
    static format(value) {
      if (value.valueOf() > 1e3 || value.valueOf() < -1e3 || value.valueOf() < 1e-3 && value.valueOf() > -1e-3) {
        return _ExpFormat.sToExp(_ExpFormat.f2s(value));
      }
      return _ExpFormat.f3f(value);
    }
  };

  // src/Typescript/column.ts
  var Column = class _Column {
    static {
      this.INPUT = "Input";
    }
    static {
      this.OUTPUT = "Output";
    }
    constructor(dim, dimIndex, spData, label, categories, ioType) {
      this.dimIndex = dimIndex;
      this.dim = dim;
      this.label = label;
      this.categories = categories;
      this.myDomain = extent(spData.sampleData, function(row) {
        return +row[dim];
      });
      this.ioType = ioType;
      const data = spData.sampleData.map(function(row) {
        return row[dim];
      });
      this.sd = deviation(data);
      const sorteddata = data.filter((d) => d !== null && !isNaN(d)).sort(ascending);
      this.p25 = quantile(sorteddata, 0.25);
      this.p75 = quantile(sorteddata, 0.75);
    }
    domain() {
      if (typeof this.myDomain[0] === "undefined" || typeof this.myDomain[1] === "undefined") {
        console.error("Wrong domain for ", this.dim);
        return [0, 1];
      }
      if (this.categories === null) {
        return this.myDomain;
      } else {
        return [this.myDomain[0] - 0.4, this.myDomain[1] + 0.6];
      }
    }
    formatedRowValue(row) {
      return this.formatedValue(row[this.dim]);
    }
    formatedValue(value) {
      if (this.categories) {
        if (value >= 0 && value < this.categories.length) {
          return Number.isInteger(value.valueOf()) ? this.categories[value.valueOf()].toString() : "";
        }
        console.warn(value, " is not valid, it should be between 0 and ", this.categories.length);
        return "";
      } else {
        return ExpFormat.format(value);
      }
    }
    axisTicks() {
      if (this.categories) {
        return this.categories.length;
      } else {
        return 4;
      }
    }
    labelText() {
      return this.label.replace(/<br>/gi, " ");
    }
    isInput() {
      return this.ioType === _Column.INPUT;
    }
    isOutput() {
      return this.ioType === _Column.OUTPUT;
    }
  };

  // src/Typescript/rowFilter.ts
  var RowFilter = class {
    constructor(xDim, yDim) {
      this.xyCutoffs = null;
      this.xDim = xDim;
      this.yDim = yDim;
    }
    isKeptRow(row) {
      return this.isKeptValue(row[this.xDim], row[this.yDim]);
    }
    isKeptValue(xValue, yValue) {
      if (this.xyCutoffs !== null) {
        let active = false;
        this.xyCutoffs.forEach(function(xyCutoff) {
          const xActive = xyCutoff[0] === null || xyCutoff[0][0] <= xValue && xValue <= xyCutoff[0][1];
          const yActive = xyCutoff[1] === null || xyCutoff[1][0] <= yValue && yValue <= xyCutoff[1][1];
          active = active || xActive && yActive;
        });
        return active;
      }
      return true;
    }
  };

  // src/Typescript/spConst.ts
  var SpConst = class _SpConst {
    static {
      this.CONTINUOUS_CS = {
        // From d3-scale.
        Viridis: viridis_default,
        Inferno: inferno,
        Magma: magma,
        Plasma: plasma,
        Warm: warm,
        Cool: cool,
        Rainbow: rainbow_default,
        CubehelixDefault: cubehelix_default2,
        // From d3-scale-chromatic
        Blues: Blues_default,
        Greens: Greens_default,
        Greys: Greys_default,
        Oranges: Oranges_default,
        Purples: Purples_default,
        Reds: Reds_default,
        BuGn: BuGn_default,
        BuPu: BuPu_default,
        GnBu: GnBu_default,
        OrRd: OrRd_default,
        PuBuGn: PuBuGn_default,
        PuBu: PuBu_default,
        PuRd: PuRd_default,
        RdBu: RdBu_default,
        RdPu: RdPu_default,
        YlGnBu: YlGnBu_default,
        YlGn: YlGn_default,
        YlOrBr: YlOrBr_default,
        YlOrRd: YlOrRd_default
      };
    }
    static {
      this.CONTINUOUS_CS_IDS = Object.keys(_SpConst.CONTINUOUS_CS);
    }
    static {
      this.CATEGORIAL_CS = {
        Category10: ordinal(category10_default),
        Accent: ordinal(Accent_default),
        Dark2: ordinal(Dark2_default),
        Paired: ordinal(Paired_default),
        Set1: ordinal(Set1_default)
      };
    }
    static {
      this.CATEGORIAL_CS_IDS = Object.keys(_SpConst.CATEGORIAL_CS);
    }
    static {
      this.dblClickDelay = 350;
    }
    static {
      this.histogramRep = {
        key: "histogramRep",
        label: "Histogram"
      };
    }
    static {
      this.densityRep = {
        key: "densityRep",
        label: "Density Plot"
      };
    }
    static {
      this.distribRepList = [_SpConst.histogramRep, _SpConst.densityRep];
    }
    static {
      this.tooltipMouse = {
        key: "tooltip",
        label: "Tooltip"
      };
    }
    static {
      this.filterMouse = {
        key: "filter",
        label: "Filter"
      };
    }
    static {
      this.zoomMouse = {
        key: "zoom",
        label: "Zoom"
      };
    }
    static {
      this.mouseModeList = [_SpConst.tooltipMouse, _SpConst.filterMouse, _SpConst.zoomMouse];
    }
    static {
      this.CAT_RATIO = 4;
    }
  };

  // src/Typescript/spData.ts
  var SpData = class _SpData {
    // eslint-disable-next-line max-lines-per-function
    constructor(config) {
      this.dimensions = [];
      this.rowLabels = null;
      this.sampleData = [];
      this.cutSampleData = null;
      this.columns = {};
      // Column for each dimension
      this.jitterXValues = [];
      this.jitterYValues = [];
      this.cutRows = [];
      this.hlPointIndex = null;
      this.dispatch = dispatch_default(_SpData.PLOT_EVENT, _SpData.ROW_FILTER_EVENT, _SpData.HL_POINT_EVENT, _SpData.MOUSE_OVER_POINT_EVENT, _SpData.HL_GRAPH_EVENT);
      /** Map which associates to a x axis dim a map which associates to a y axis dim a 'RowFilter' (a list of couples of cutoffs) */
      this.rowFilterMap = /* @__PURE__ */ new Map();
      this.rowLabels = config.rowLabels;
      const thisData = this;
      _SpData.checkData(config);
      _SpData.checkCategorical(config);
      _SpData.checkColumnLabels(config);
      _SpData.checkInputColumns(config);
      this.sampleData = [];
      config.data.forEach(function(r) {
        const curRow = {};
        config.data.columns.forEach((dim, i) => {
          const categories = Array.isArray(config.categorical) ? config.categorical[i] : null;
          const cellValue = r[dim];
          if (typeof cellValue === "undefined") {
            curRow[dim] = NaN;
          } else if (categories) {
            let catIndex = categories.indexOf(cellValue.toString());
            if (catIndex === -1) {
              catIndex = categories.indexOf(+cellValue);
            }
            curRow[dim] = catIndex === -1 ? NaN : catIndex;
          } else {
            curRow[dim] = +cellValue;
          }
        });
        thisData.sampleData.push(curRow);
      });
      if (!this.checkCutoffs(config.cutoffs)) {
        config.cutoffs = null;
      }
      if (Array.isArray(config.cutoffs)) {
        config.cutoffs.forEach((spCutoffs) => {
          this.setXYCutoffs(spCutoffs.xDim, spCutoffs.yDim, spCutoffs.xyCutoffs);
        });
      }
      const allDimensions = Object.keys(this.sampleData[0]);
      const nanColumns = allDimensions.map((dim) => this.sampleData.every((row) => isNaN(row[dim])));
      this.dimensions = allDimensions.filter(
        (_dim, i) => !(nanColumns[i] || Array.isArray(config.keptColumns) && !config.keptColumns[i])
      );
      allDimensions.forEach((dim, i) => {
        const isInput = Array.isArray(config.inputColumns) ? config.inputColumns[i] : true;
        this.columns[dim] = new Column(
          dim,
          i,
          this,
          Array.isArray(config.columnLabels) ? config.columnLabels[i] : dim,
          Array.isArray(config.categorical) ? config.categorical[i] : null,
          isInput ? Column.INPUT : Column.OUTPUT
        );
        if (this.columns[dim].categories !== null && !thisData.jitterXValues.length) {
          thisData.jitterXValues = this.sampleData.map((_row) => (Math.random() - 0.5) / SpConst.CAT_RATIO);
          thisData.jitterYValues = this.sampleData.map((_row) => (Math.random() - 0.5) / SpConst.CAT_RATIO);
        }
      });
    }
    static {
      this.PLOT_EVENT = "plotEvent";
    }
    static {
      this.ROW_FILTER_EVENT = "rowFilterEvent";
    }
    static {
      this.HL_POINT_EVENT = "hlPointEvent";
    }
    static {
      this.MOUSE_OVER_POINT_EVENT = "mouseOverPointEvent";
    }
    static {
      this.HL_GRAPH_EVENT = "hlGraphEvent";
    }
    static {
      this.CUTOFF_EVENT = "cutoffChange";
    }
    static {
      this.POINT_CLICK_EVENT = "pointClicked";
    }
    static checkData(config) {
      if (!Array.isArray(config.data)) {
        throw new Error("given dataset is not a D3 friendly (row-oriented) data");
      }
      if (config.data.length === 0) {
        throw new Error("given dataset contains no line)");
      }
      if (typeof config.data.columns === "undefined") {
        config.data.columns = Object.keys(config.data[0]);
      }
    }
    static checkCategorical(config) {
      if (config.categorical) {
        if (Array.isArray(config.categorical)) {
          if (config.categorical.length !== config.data.columns.length) {
            console.error("Length of 'categorical' must be equal to the number of columns of 'data'");
            config.categorical = null;
          }
        } else {
          console.error("'categorical' must be an array");
          config.categorical = null;
        }
      }
    }
    static checkColumnLabels(config) {
      if (config.columnLabels) {
        if (Array.isArray(config.columnLabels)) {
          if (config.columnLabels.length !== config.data.columns.length) {
            console.error("Length of 'columnLabels' must be equal to the number of columns of 'data'");
            config.columnLabels = null;
          }
        } else {
          console.error("'columnLabels' must be an array");
          config.columnLabels = null;
        }
      }
    }
    static checkInputColumns(config) {
      if (config.inputColumns) {
        if (Array.isArray(config.inputColumns)) {
          if (config.inputColumns.length !== config.data.columns.length) {
            console.error("Length of 'inputColumns' must be equal to the number of columns of 'data'");
            config.inputColumns = null;
          }
        } else {
          console.error("'inputColumns' must be an array");
          config.inputColumns = null;
        }
      }
    }
    checkCutoffs(cutoffs) {
      if (cutoffs) {
        if (Array.isArray(cutoffs)) {
          const allDimensions = Object.keys(this.sampleData[0]);
          for (let spcIndex = 0; spcIndex < cutoffs.length; spcIndex++) {
            const spCutoffs = cutoffs[spcIndex];
            if (typeof spCutoffs.xDim === "undefined") {
              console.error(`spCutoffs ${spcIndex} has no 'xDim' attribute`);
              return false;
            }
            if (allDimensions.indexOf(spCutoffs.xDim) === -1) {
              console.error(`spCutoffs ${spcIndex} has unknown 'xDim' attribute: ${spCutoffs.xDim}`);
              return false;
            }
            if (typeof spCutoffs.yDim === "undefined") {
              console.error(`spCutoffs ${spcIndex} has no 'yDim' attribute`);
              return false;
            }
            if (allDimensions.indexOf(spCutoffs.yDim) === -1) {
              console.error(`spCutoffs ${spcIndex} has unknown 'yDim' attribute: ${spCutoffs.yDim}`);
              return false;
            }
            if (typeof spCutoffs.xyCutoffs === "undefined") {
              console.error(`spCutoffs ${spcIndex} has no 'xyCutoffs' attribute`);
              return false;
            }
            if (!_SpData.checkXYCutoffs(spCutoffs.xyCutoffs, spcIndex)) {
              return false;
            }
          }
        } else {
          console.error("'cutoffs' must be an array");
          return false;
        }
      }
      return true;
    }
    static checkXYCutoffs(xyCutoffs, spcIndex) {
      if (Array.isArray(xyCutoffs)) {
        for (let i = 0; i < xyCutoffs.length; i++) {
          const xyCutoff = xyCutoffs[i];
          if (Array.isArray(xyCutoff)) {
            if (!_SpData.checkXYCutoff(xyCutoff, spcIndex)) {
              return false;
            }
          } else {
            console.error("cutoff with a non array 'xyCutoff' found");
            return false;
          }
        }
      } else {
        console.error(`spCutoffs ${spcIndex} has an invalid 'xyCutoffs' (not an array)`);
        return false;
      }
      return true;
    }
    static checkXYCutoff(xyCutoff, spcIndex) {
      if (xyCutoff.length === 2) {
        for (let xyi = 0; xyi < xyCutoff.length; xyi++) {
          const cutoff = xyCutoff[xyi];
          if (Array.isArray(cutoff)) {
            if (!_SpData.checkCutoff(cutoff, spcIndex, xyi)) {
              xyCutoff[xyi] = null;
            }
          } else if (cutoff) {
            console.error(`spCutoffs ${spcIndex} has an invalid 'cutoff' (not an array of length 2)`);
            return false;
          }
        }
      } else {
        console.error(`spCutoffs ${spcIndex} has an invalid 'xyCutoff' (length is not 2)`);
        return false;
      }
      return true;
    }
    static checkCutoff(cutoff, spcIndex, xyi) {
      if (cutoff.length !== 2) {
        console.error(`spCutoffs ${spcIndex} has an invalid 'cutoff' (length is not 2)`);
        return false;
      } else if (+cutoff[0] === +cutoff[1]) {
        console.error(`spCutoffs ${spcIndex} has an invalid 'cutoff' for ${xyi === 0 ? "X" : "Y"} (${cutoff[0]} === ${+cutoff[1]})`);
        return false;
      }
      return true;
    }
    updateCutRowsAttributes() {
      const thisData = this;
      if (this.cutRows.length !== this.sampleData.length) {
        this.cutRows = new Array(this.sampleData.length);
      }
      const rowFilterList = [...this.rowFilterMap.values()].reduce(
        (accu, xmap) => accu.concat([...xmap.values()]),
        []
      );
      this.sampleData.forEach(function(row, i) {
        const isKept = rowFilterList.every(function(rowFilter) {
          return rowFilter.isKeptRow(row);
        });
        thisData.cutRows[i] = isKept;
      });
      this.cutSampleData = null;
    }
    cutData() {
      const thisData = this;
      if (this.cutSampleData === null) {
        this.cutSampleData = this.sampleData.filter((_row, i) => thisData.cutRows[i]);
      }
      return this.cutSampleData;
    }
    changeHlPoint(pointIndex, scatterPlot, fireMouseEvent) {
      if (this.hlPointIndex !== pointIndex) {
        this.hlPointIndex = pointIndex;
        this.dispatch.call(_SpData.HL_POINT_EVENT, void 0, { pointIndex, scatterPlot });
        if (fireMouseEvent) {
          this.dispatch.call(_SpData.MOUSE_OVER_POINT_EVENT, void 0, { pointIndex, scatterPlot });
        }
      }
    }
    dispatchRowFilterEvent() {
      this.dispatch.call(_SpData.ROW_FILTER_EVENT, void 0, _SpData.ROW_FILTER_EVENT);
    }
    sendCutoffEvent(adjusting) {
      this.dispatch.call(
        _SpData.PLOT_EVENT,
        void 0,
        {
          type: _SpData.CUTOFF_EVENT,
          value: { adjusting, cutoffs: this.getXYCutoffs() }
        }
      );
    }
    sendClickEvent(pointIndex) {
      this.dispatch.call(
        _SpData.PLOT_EVENT,
        void 0,
        {
          type: _SpData.POINT_CLICK_EVENT,
          value: { pointIndex }
        }
      );
    }
    sendHlPointEvent(pointIndex) {
      this.dispatch.call(
        _SpData.PLOT_EVENT,
        void 0,
        {
          type: _SpData.HL_POINT_EVENT,
          value: { pointIndex }
        }
      );
    }
    on(typenames, callback) {
      this.dispatch.on(typenames, callback);
    }
    setCutoffs(spCutoffsList) {
      this.rowFilterMap.clear();
      if (spCutoffsList) {
        if (this.checkCutoffs(spCutoffsList)) {
          spCutoffsList.forEach((spCutoffs) => {
            this.setXYCutoffs(spCutoffs.xDim, spCutoffs.yDim, spCutoffs.xyCutoffs);
          });
          this.dispatchRowFilterEvent();
        }
      } else {
        this.dispatchRowFilterEvent();
      }
    }
    getRowFilter(xDim, yDim) {
      let xMap = this.rowFilterMap.get(xDim);
      if (!xMap) {
        xMap = /* @__PURE__ */ new Map();
        this.rowFilterMap.set(xDim, xMap);
      }
      let rowFilter = xMap.get(yDim);
      if (!rowFilter) {
        rowFilter = new RowFilter(xDim, yDim);
        xMap.set(yDim, rowFilter);
      }
      return rowFilter;
    }
    // eslint-disable-next-line max-lines-per-function
    setXYCutoffs(xDim, yDim, xyCutoffs) {
      if (xyCutoffs) {
        const [xMin, xMax] = extent(this.sampleData, function(row) {
          return +row[xDim];
        });
        if (typeof xMin !== "undefined" && typeof xMax !== "undefined") {
          xyCutoffs.forEach((xyCutoff) => {
            if (xyCutoff[0]) {
              if (xyCutoff[0][0] > xyCutoff[0][1]) {
                const tmp = xyCutoff[0][0];
                xyCutoff[0][0] = xyCutoff[0][1];
                xyCutoff[0][1] = tmp;
              }
              if (xyCutoff[0][0] < xMin || xyCutoff[0][0] > xMax) {
                xyCutoff[0][0] = xMin;
              }
              if (xyCutoff[0][1] > xMax || xyCutoff[0][1] < xMin) {
                xyCutoff[0][1] = xMax;
              }
            } else {
              xyCutoff[0] = [xMin, xMax];
            }
          });
        } else {
          console.error("Wrong domain for ", xDim);
        }
        const [yMin, yMax] = extent(this.sampleData, function(row) {
          return +row[yDim];
        });
        if (typeof yMin !== "undefined" && typeof yMax !== "undefined") {
          xyCutoffs.forEach((xyCutoff) => {
            if (xyCutoff[1]) {
              if (xyCutoff[1][0] > xyCutoff[1][1]) {
                const tmp = xyCutoff[1][0];
                xyCutoff[1][0] = xyCutoff[1][1];
                xyCutoff[1][1] = tmp;
              }
              if (xyCutoff[1][0] < yMin || xyCutoff[1][0] > yMax) {
                xyCutoff[1][0] = yMin;
              }
              if (xyCutoff[1][1] > yMax || xyCutoff[1][1] < yMin) {
                xyCutoff[1][1] = yMax;
              }
            } else {
              xyCutoff[1] = [yMin, yMax];
            }
          });
        } else {
          console.error("Wrong domain for ", yDim);
        }
      }
      this.getRowFilter(xDim, yDim).xyCutoffs = xyCutoffs;
    }
    getXYCutoffs() {
      const rowFilterList = [...this.rowFilterMap.values()].reduce(
        (accu, xmap) => accu.concat([...xmap.values()]),
        []
      );
      return rowFilterList.filter((rowFilter) => rowFilter.xyCutoffs !== null).map((rowFilter) => {
        return {
          xDim: rowFilter.xDim,
          yDim: rowFilter.yDim,
          xyCutoffs: rowFilter.xyCutoffs
        };
      });
    }
  };

  // src/Typescript/corrPlot.ts
  var CorrPlot = class _CorrPlot {
    constructor(spData, config) {
      this.xPlot = 0;
      this.yPlot = 0;
      this.width = 0;
      this.height = 0;
      this.axisVisibility = { xTitle: true, xValues: true, yTitle: true, yValues: true };
      this.repType = _CorrPlot.CIRCLES_REP;
      this.spData = spData;
      this.bindto = config.bindto;
      this.index = config.index;
      this.xColumn = spData.columns[spData.dimensions[0]];
      this.yColumn = spData.columns[spData.dimensions[0]];
      this.zColumn = null;
      this.row = config.row;
      this.col = config.col;
      this.corrCsId = config.corrPlotCsId;
      this.categoricalCsId = config.categoricalCsId;
      this.axisVisibility = config.axisVisibility;
      this.repType = config.corrPlotType;
      this.style = config.style;
      this.catColorScale = SpConst.CATEGORIAL_CS[this.categoricalCsId];
    }
    static {
      this.EMPTY_REP = "Empty";
    }
    static {
      this.CIRCLES_REP = "Circles";
    }
    static {
      this.TEXT_REP = "Text";
    }
    static {
      this.ABS_TEXT_REP = "AbsText";
    }
    static {
      this.padding = { r: 10, t: 10 };
    }
    setXColumn(column) {
      this.xColumn = column;
    }
    setYColumn(column) {
      this.yColumn = column;
    }
    setZColumn(column) {
      this.zColumn = column;
    }
    formatXValue(value) {
      return this.xColumn.formatedValue(value);
    }
    formatYValue(value) {
      return this.yColumn.formatedValue(value);
    }
    formatZValue(value) {
      return this.zColumn === null ? "No Z axis" : this.zColumn.formatedValue(value);
    }
    // eslint-disable-next-line max-lines-per-function
    draw(updateType) {
      const plotSelection = this.plotSelection();
      plotSelection.select(".corrPlotArea").remove();
      if (this.repType === _CorrPlot.EMPTY_REP) {
        return;
      }
      this.updateZScale();
      const areaSelection = plotSelection.append("g").attr("class", "corrPlotArea").attr("transform", "translate(0," + _CorrPlot.padding.t + ")");
      const cpBorder = areaSelection.append("rect").attr("class", "cpBorder").attr("width", this.width - _CorrPlot.padding.r).attr("height", this.height - _CorrPlot.padding.t).on("mouseover", () => {
        this.spData.dispatch.call(SpData.HL_GRAPH_EVENT, void 0, this);
      }).on("mouseout", (event) => {
        const coord = pointer_default(event);
        if (coord[0] < 0 || coord[0] > this.width || coord[1] < 0 || coord[0] > this.width) {
          this.spData.dispatch.call(SpData.HL_GRAPH_EVENT, void 0, null);
        }
      });
      if (this.xColumn.categories === null && this.yColumn.categories === null) {
        if (this.repType === _CorrPlot.CIRCLES_REP) {
          this.drawCCTreemap(updateType, areaSelection);
        } else {
          this.drawCorrValues(updateType, areaSelection);
          cpBorder.classed("cpNaBorder", true);
        }
      } else {
        this.drawNA(updateType, areaSelection);
        cpBorder.classed("cpNaBorder", true);
      }
      const mspDivNode = select_default2(this.bindto + " .mspDiv").node();
      const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
      const xParent = parentBounds === null ? 0 : parentBounds.x;
      const yParent = parentBounds === null ? 0 : parentBounds.y;
      const spRectNode = cpBorder.node();
      const spRectBounds = spRectNode === null ? null : spRectNode.getBoundingClientRect();
      const xSpRect = spRectBounds === null ? 0 : spRectBounds.x;
      const ySpRect = spRectBounds === null ? 0 : spRectBounds.y;
      this.xPlot = xSpRect - xParent;
      this.yPlot = ySpRect - yParent;
    }
    hlGraph(highlight) {
      const plotSelection = this.plotSelection();
      if (this.xColumn.categories === null && this.yColumn.categories === null) {
        plotSelection.select(".cpBorder").classed("hlGraph", highlight);
      } else {
        plotSelection.select(".cpBorder").classed("hlGraph", highlight);
      }
    }
    plotSelection(plotSelection) {
      if (plotSelection) {
        return plotSelection;
      }
      const thisPlot = this;
      const mspGroup = select_default2(this.bindto + " .mspGroup");
      return mspGroup.selectAll(".corrPlot").filter(function(plot) {
        return plot.row === thisPlot.row && plot.col === thisPlot.col;
      });
    }
    // eslint-disable-next-line max-lines-per-function
    drawCCTreemap(_updateType, areaSelection) {
      const thisPlot = this;
      const corrValues = this.corrValues();
      const packLayout = pack_default().size([this.width - _CorrPlot.padding.r, this.height - _CorrPlot.padding.t]).padding(2);
      const rootNode = hierarchy(corrValues);
      rootNode.sum(function(_cv) {
        return 1;
      });
      packLayout(rootNode);
      const categoriesCount = this.zColumn !== null && this.zColumn.categories ? this.zColumn.categories.length : 0;
      areaSelection.selectAll("circle.corrValues").data(rootNode.descendants()).enter().filter((node) => categoriesCount !== 0 || node.data.clazz !== "Root").append("circle").style("pointer-events", "none").attr("class", (node) => "corrValues " + node.data.clazz).attr("cx", (node) => {
        const cx = node.x;
        if (isNaN(cx)) {
          console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cx is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
          return 0;
        }
        return cx;
      }).attr("cy", (node) => {
        const cy = node.y;
        if (isNaN(cy)) {
          console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cy is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
          return 0;
        }
        return cy;
      }).attr("r", (node) => {
        const r = node.r * Math.abs(node.data.value);
        return isFinite(r) ? r : 0;
      }).attr("stroke", "none").filter((node) => node.data.clazz !== "Root").attr("fill", function(node) {
        if (node.data.clazz === "All") {
          return thisPlot.style.plotProperties.noCatColor;
        }
        return thisPlot.catColorScale(node.data.catIndex);
      });
      areaSelection.selectAll("circle.cvBorder").data(rootNode.descendants()).enter().filter((node) => node.data.clazz !== "Root").append("circle").attr("class", (node) => "cvBorder " + node.data.clazz).attr("cx", (node) => {
        const cx = node.x;
        if (isNaN(cx)) {
          if (node.data.keptCount > 1) {
            console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cx is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
          }
          return 0;
        }
        return cx;
      }).attr("cy", (node) => {
        const cy = node.y;
        if (isNaN(cy)) {
          if (node.data.keptCount > 1) {
            console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cy is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
          }
          return 0;
        }
        return cy;
      }).attr("r", (node) => {
        const r = node.r;
        if (isNaN(r)) {
          if (node.data.keptCount > 1) {
            console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), r is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
          }
          return 0;
        }
        return r;
      }).attr("fill", "none").attr("stroke", categoriesCount ? "black" : "none").on("mouseover", function(_event, node) {
        thisPlot.mouseoverCircle(node);
      }).on("mouseout", function() {
        thisPlot.mouseout();
      });
    }
    // eslint-disable-next-line max-lines-per-function
    drawCorrValues(_updateType, areaSelection) {
      const thisPlot = this;
      const corrValues = this.corrValues().children;
      const categoriesCount = this.zColumn !== null && this.zColumn.categories ? this.zColumn.categories.length : 0;
      const contColorScale = sequential(SpConst.CONTINUOUS_CS[this.corrCsId]).domain(thisPlot.repType === _CorrPlot.TEXT_REP ? [-1, 1] : [0, 1]);
      const corrGroup = areaSelection.selectAll("g.corrGroup").data(corrValues).enter().append("g").attr("class", "corrGroup").on("mouseover", function(_event, cv) {
        thisPlot.mouseoverText(cv);
      }).on("mouseout", function() {
        thisPlot.mouseout();
      });
      const valueText = corrGroup.append("text").attr("class", "corrNumber").style("fill", function(cv) {
        if (categoriesCount) {
          return cv.clazz === "All" ? thisPlot.style.plotProperties.noCatColor : thisPlot.catColorScale(cv.catIndex);
        } else {
          const color2 = contColorScale(
            thisPlot.repType === _CorrPlot.ABS_TEXT_REP ? Math.abs(cv.value) : cv.value
          );
          return color2 ?? SpConst.CONTINUOUS_CS[thisPlot.corrCsId](0);
        }
      });
      if (categoriesCount) {
        const bandScale = band().domain(range(categoriesCount + 1)).range([0, this.height - _CorrPlot.padding.t]).paddingInner(0.3).paddingOuter(1);
        corrGroup.append("rect").attr("class", "corrCat").attr("x", bandScale.bandwidth() / 2).attr("y", (_cv, i) => bandScale(i) ?? 0).attr("width", bandScale.bandwidth()).attr("height", bandScale.bandwidth()).style(
          "fill",
          (cv) => cv.clazz === "All" ? "none" : thisPlot.catColorScale(cv.catIndex)
        );
        valueText.attr("x", bandScale.bandwidth() * 2).attr("y", (_cv, i) => bandScale(i) ?? 0).attr("dy", bandScale.bandwidth() * 0.6).attr("font-size", bandScale.bandwidth()).attr("dominant-baseline", "middle").text((cv) => `${ExpFormat.format(cv.value)}`);
      } else {
        valueText.attr("x", (this.width - _CorrPlot.padding.r) / 2).attr("y", (this.height - _CorrPlot.padding.t) / 2).attr("font-size", (this.height - _CorrPlot.padding.t) / 5 + "px").attr("text-anchor", "middle").attr("dominant-baseline", "middle").text((cv) => `${ExpFormat.format(cv.value)}`);
      }
    }
    mouseoverText(corrValues) {
      this.spData.dispatch.call(SpData.HL_GRAPH_EVENT, void 0, this);
      const tooltipLocation = this.tooltipLocation();
      select_default2(this.bindto + " .mspTooltip").remove();
      const mspDiv = select_default2(this.bindto + " .mspDiv");
      const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
      tooltip.append("div").attr("class", "title").html("Correlation Values");
      switch (corrValues.clazz) {
        case "All":
          this.updateTooltipWithAllCorrelation(corrValues);
          break;
        case "Cat":
          this.updateTooltipWithCatCorrelation(corrValues);
          break;
        default:
          break;
      }
      this.updateXYTooltip();
    }
    tooltipLocation() {
      const mspDivNode = select_default2(this.bindto + " .mspDiv").node();
      const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
      const xParent = parentBounds === null ? 0 : parentBounds.x;
      const plotGroup = select_default2(this.bindto + " .mspGroup").node();
      const elementBounds = plotGroup === null ? null : plotGroup.getBoundingClientRect();
      const xRect = elementBounds === null ? 0 : elementBounds.x;
      const wRect = elementBounds === null ? 0 : elementBounds.width;
      return [xRect - xParent + wRect + 5, this.yPlot];
    }
    drawNA(_updateType, areaSelection) {
      const corrText = "NA";
      areaSelection.append("text").attr("class", "naCorrNumber").attr("x", (this.width - _CorrPlot.padding.r) / 2).attr("y", (this.height - _CorrPlot.padding.t) / 2).attr("font-size", (this.height - _CorrPlot.padding.t) / 5 + "px").attr("text-anchor", "middle").attr("dominant-baseline", "middle").text(corrText);
    }
    // eslint-disable-next-line max-lines-per-function
    corrValues() {
      const corrValues = {
        label: "Root",
        catIndex: NaN,
        clazz: "Root",
        children: [],
        value: NaN,
        pointsCount: NaN,
        keptCount: NaN
      };
      const corr = _CorrPlot.getPearsonCorrelation(
        this.spData.cutData().map((row) => row[this.xColumn.dim]),
        this.spData.cutData().map((row) => row[this.yColumn.dim])
      );
      corrValues.children.push({
        label: "All",
        catIndex: NaN,
        clazz: "All",
        children: [],
        value: corr,
        pointsCount: this.spData.sampleData.length,
        keptCount: this.spData.cutData().length
      });
      let corrCount = 1;
      const zColumn = this.zColumn;
      if (zColumn !== null && zColumn.categories) {
        const categories = zColumn.categories;
        categories.forEach((cat, i) => {
          const filteredData = this.spData.cutData().filter((row) => {
            const catIndex = row[zColumn.dim];
            return categories[catIndex] === cat;
          });
          const catCorr = _CorrPlot.getPearsonCorrelation(
            filteredData.map((row) => row[this.xColumn.dim]),
            filteredData.map((row) => row[this.yColumn.dim])
          );
          if (isFinite(catCorr)) {
            const catData = this.spData.sampleData.filter((row) => {
              const catIndex = row[zColumn.dim];
              return categories[catIndex] === cat;
            });
            corrValues.children.push({
              label: (i + 1).toString(),
              catIndex: i,
              clazz: "Cat",
              children: [],
              value: catCorr,
              pointsCount: catData.length,
              keptCount: filteredData.length
            });
            corrCount = corrCount + 1;
          }
        });
      }
      return corrValues;
    }
    // eslint-disable-next-line max-lines-per-function
    mouseoverCircle(node) {
      this.spData.dispatch.call(SpData.HL_GRAPH_EVENT, void 0, this);
      const tooltipLocation = this.tooltipLocation();
      select_default2(this.bindto + " .mspTooltip").remove();
      const mspDiv = select_default2(this.bindto + " .mspDiv");
      const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
      tooltip.append("div").attr("class", "title").html("Correlation Circular Treemap");
      if (typeof node.data === "undefined") {
        return;
      }
      switch (node.data.clazz) {
        case "All":
          this.updateTooltipWithAllCorrelation(node.data);
          break;
        case "Cat":
          this.updateTooltipWithCatCorrelation(node.data);
          break;
        default:
          break;
      }
      this.updateXYTooltip();
    }
    updateXYTooltip() {
      const subTipDiv = select_default2(this.bindto + " .mspTooltip").append("div").attr("class", "subTipDiv");
      subTipDiv.append("div").html(`x: ${this.xColumn.label.replace(/<br>/gi, " ")}`);
      subTipDiv.append("div").html(`y: ${this.yColumn.label.replace(/<br>/gi, " ")}`);
    }
    updateTooltipWithAllCorrelation(corrValues) {
      const subTipDiv = select_default2(this.bindto + " .mspTooltip").append("div").attr("class", "subTipDiv");
      subTipDiv.append("div").html(`<span class='swatch' style='background:"none"'>&nbsp;</span> <span class='strongValue'>${ExpFormat.format(corrValues.value)}</span>`);
      const zColumn = this.zColumn;
      if (zColumn !== null && zColumn.categories) {
        subTipDiv.append("div").html(`Correlation, whatever the value of ${zColumn.label.replace(/<br>/gi, " ")}`);
      }
      subTipDiv.append("div").html(`${corrValues.keptCount} sample points (${corrValues.pointsCount - corrValues.keptCount} filtered points)`);
    }
    updateTooltipWithCatCorrelation(corrValues) {
      const zColumn = this.zColumn;
      if (!zColumn || !zColumn.categories) {
        console.error("'mouseoverCircleCat' called, but Z column is not categorial");
        return;
      }
      const thisPlot = this;
      const subTipDiv = select_default2(this.bindto + " .mspTooltip").append("div").attr("class", "subTipDiv");
      const category = zColumn.categories[corrValues.catIndex];
      const color2 = thisPlot.catColorScale(corrValues.catIndex);
      subTipDiv.append("div").html(`<span class='swatch' style='background:${color2}'>&nbsp;</span> <span class='strongValue'>${ExpFormat.format(corrValues.value)}</span>`);
      subTipDiv.append("div").html(`Correlation when ${zColumn.labelText()} is: ${_CorrPlot.brIze(category)}`);
      subTipDiv.append("div").html(`${corrValues.keptCount} sample points (${corrValues.pointsCount - corrValues.keptCount} filtered points)`);
    }
    static brIze(category) {
      const splittedCategory = category.toString().split(", ");
      if (splittedCategory.length > 1) {
        return "<br>" + splittedCategory.join(",<br>") + "<br>";
      }
      return splittedCategory[0];
    }
    mouseout() {
      select_default2(this.bindto + " .mspTooltip").style("display", "none");
    }
    updateZScale() {
      const zColumn = this.zColumn;
      if (zColumn !== null && zColumn.categories !== null) {
        const zMax = zColumn.domain()[1];
        this.catColorScale = SpConst.CATEGORIAL_CS[this.categoricalCsId].domain(range(zMax));
      }
    }
    static getPearsonCorrelation(x2, y2) {
      let shortestArrayLength = 0;
      if (x2.length === y2.length) {
        shortestArrayLength = x2.length;
      } else if (x2.length > y2.length) {
        shortestArrayLength = y2.length;
        console.error("x has more items in it, the last " + (x2.length - shortestArrayLength) + " item(s) will be ignored");
      } else {
        shortestArrayLength = x2.length;
        console.error("y has more items in it, the last " + (y2.length - shortestArrayLength) + " item(s) will be ignored");
      }
      let sum_x = 0;
      let sum_y = 0;
      let sum_xy = 0;
      let sum_x2 = 0;
      let sum_y2 = 0;
      for (let i = 0; i < shortestArrayLength; i++) {
        sum_x += x2[i];
        sum_y += y2[i];
        sum_xy += x2[i] * y2[i];
        sum_x2 += x2[i] * x2[i];
        sum_y2 += y2[i] * y2[i];
      }
      const step1 = shortestArrayLength * sum_xy - sum_x * sum_y;
      const step2 = shortestArrayLength * sum_x2 - sum_x * sum_x;
      const step3 = shortestArrayLength * sum_y2 - sum_y * sum_y;
      const step4 = Math.sqrt(step2 * step3);
      const answer = step1 / step4;
      return isFinite(answer) ? answer : NaN;
    }
  };

  // src/Typescript/distributionData.ts
  var DistributionData = class _DistributionData {
    constructor(distributionPlot, zCatDescription) {
      this.uncutDensityScale = linear2();
      this.uncutDensity = null;
      this.cutDensityScale = linear2();
      this.cutDensity = null;
      this.uncutHistoScale = linear2();
      this.uncutBins = null;
      this.cutHistoScale = linear2();
      this.cutBins = null;
      this.plot = distributionPlot;
      this.zCatDescription = zCatDescription;
    }
    computePlot(violinCatDescription) {
      this.violinCatDescription = violinCatDescription;
      const filteredUncutData = this.filterData(this.plot.spData.sampleData);
      this.computeUncutPlot(filteredUncutData);
      const filteredCutData = this.filterData(this.plot.spData.cutData());
      this.computeCutPlot(filteredCutData);
      if (filteredCutData.length > filteredUncutData.length / 2) {
        if (this.plot.useDensityRep()) {
          const max3 = max([this.cutDensityScale.domain(), this.uncutDensityScale.domain()], (d) => d[1]);
          if (max3) {
            this.uncutDensityScale.domain([0, max3]);
            this.cutDensityScale.domain([0, max3]);
          }
        }
        if (this.plot.useHistogramRep()) {
          this.cutHistoScale.domain(this.uncutHistoScale.domain());
        }
      }
      return this;
    }
    filterData(data) {
      const violinCatDescription = this.violinCatDescription;
      const filtered = violinCatDescription ? data.filter(function(row) {
        return row[violinCatDescription.column.dim] === violinCatDescription.catIndex;
      }) : data;
      const zCatDescription = this.zCatDescription;
      return zCatDescription ? filtered.filter(function(row) {
        return row[zCatDescription.column.dim] === zCatDescription.catIndex;
      }) : filtered;
    }
    computeUncutPlot(uncutData) {
      const thisData = this;
      const data = uncutData.map(function(row) {
        return row[thisData.plot.column.dim];
      });
      if (this.plot.useDensityRep()) {
        this.uncutDensity = this.computeDensityPlot(data, this.uncutDensityScale);
      }
      if (this.plot.useHistogramRep()) {
        this.uncutBins = this.computeHistogramPlot(data, this.uncutHistoScale);
      }
      return this;
    }
    computeCutPlot(cutData) {
      const thisData = this;
      const data = cutData.map(function(row) {
        return row[thisData.plot.column.dim];
      });
      if (this.plot.useDensityRep()) {
        this.cutDensity = this.computeDensityPlot(data, this.cutDensityScale);
      }
      if (this.plot.useHistogramRep()) {
        this.cutBins = this.computeHistogramPlot(data, this.cutHistoScale);
      }
      return this;
    }
    computeDensityPlot(data, densityScale) {
      const bandwidth = 0.9 * Math.min(
        this.plot.column.sd,
        Math.abs(this.plot.column.p75 - this.plot.column.p25) / 1.34
      ) * Math.pow(data.length, -0.2);
      const domain = this.plot.valuesScale.domain();
      const thresholds = this.equiDepthThresholds(domain[0], domain[1], true);
      const density = _DistributionData.kde(_DistributionData.epanechnikov(bandwidth), thresholds, data);
      density.push([thresholds[thresholds.length - 1], 0]);
      density.unshift([thresholds[0], 0]);
      const densityMax = max(density, (point3) => point3[1]);
      densityScale.domain([0, densityMax ? densityMax * 1.05 : 1]);
      return density;
    }
    computeHistogramPlot(data, histoScale) {
      const domain = this.plot.valuesScale.domain();
      const thresholds = this.plot.column.categories ? range(SpConst.CAT_RATIO * this.plot.column.categories.length).map((t) => (t - 0.5) / SpConst.CAT_RATIO) : this.equiDepthThresholds(domain[0], domain[1], false);
      const bins = bin().domain(domain).thresholds(thresholds)(data);
      const binMax = max(bins, (bin2) => bin2.length);
      histoScale.domain([0, binMax ? binMax : 1]);
      return bins;
    }
    equiDepthThresholds(min3, max3, includeMax) {
      const binBounds = [];
      const depth = (max3 - min3) / this.numbin();
      for (let j = 0; j < this.numbin(); j++) {
        binBounds.push(min3 + j * depth);
      }
      if (includeMax) {
        binBounds.push(max3);
      }
      return binBounds;
    }
    numbin() {
      return Math.ceil(2.5 * Math.pow(this.plot.spData.sampleData.length, 0.25));
    }
    distribScaleRange(range2) {
      this.uncutDensityScale.range(range2);
      this.uncutHistoScale.range(range2);
      this.cutDensityScale.range(range2);
      this.cutHistoScale.range(range2);
      return this;
    }
    static kde(kernel, thresholds, data) {
      return thresholds.map((t) => [t, mean(data, (d) => kernel(t - d))]);
    }
    static epanechnikov(bandwidth) {
      return (x1) => {
        const x2 = x1 / bandwidth;
        return Math.abs(x2) <= 1 ? 0.75 * (1 - x2 * x2) / bandwidth : 0;
      };
    }
  };

  // node_modules/d3-regression/dist/d3-regression.esm.js
  function _slicedToArray(arr, i) {
    return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest();
  }
  function _arrayWithHoles(arr) {
    if (Array.isArray(arr))
      return arr;
  }
  function _iterableToArrayLimit(arr, i) {
    var _arr = [];
    var _n = true;
    var _d = false;
    var _e = void 0;
    try {
      for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) {
        _arr.push(_s.value);
        if (i && _arr.length === i)
          break;
      }
    } catch (err) {
      _d = true;
      _e = err;
    } finally {
      try {
        if (!_n && _i["return"] != null)
          _i["return"]();
      } finally {
        if (_d)
          throw _e;
      }
    }
    return _arr;
  }
  function _nonIterableRest() {
    throw new TypeError("Invalid attempt to destructure non-iterable instance");
  }
  function points(data, x2, y2, sort) {
    data = data.filter(function(d2, i2) {
      var u = x2(d2, i2), v = y2(d2, i2);
      return u != null && isFinite(u) && v != null && isFinite(v);
    });
    if (sort) {
      data.sort(function(a2, b) {
        return x2(a2) - x2(b);
      });
    }
    var n = data.length, X2 = new Float64Array(n), Y2 = new Float64Array(n);
    var ux = 0, uy = 0, xv, yv, d;
    for (var i = 0; i < n; ) {
      d = data[i];
      X2[i] = xv = +x2(d, i, data);
      Y2[i] = yv = +y2(d, i, data);
      ++i;
      ux += (xv - ux) / i;
      uy += (yv - uy) / i;
    }
    for (var _i = 0; _i < n; ++_i) {
      X2[_i] -= ux;
      Y2[_i] -= uy;
    }
    return [X2, Y2, ux, uy];
  }
  function visitPoints(data, x2, y2, cb) {
    var iterations = 0;
    for (var i = 0, n = data.length; i < n; i++) {
      var d = data[i], dx = +x2(d, i, data), dy = +y2(d, i, data);
      if (dx != null && isFinite(dx) && dy != null && isFinite(dy)) {
        cb(dx, dy, iterations++);
      }
    }
  }
  function determination(data, x2, y2, uY, predict) {
    var SSE = 0, SST = 0;
    visitPoints(data, x2, y2, function(dx, dy) {
      var sse = dy - predict(dx), sst = dy - uY;
      SSE += sse * sse;
      SST += sst * sst;
    });
    return 1 - SSE / SST;
  }
  function ols(uX, uY, uXY, uX2) {
    var delta = uX2 - uX * uX, slope = Math.abs(delta) < 1e-24 ? 0 : (uXY - uX * uY) / delta, intercept = uY - slope * uX;
    return [intercept, slope];
  }
  function linear3() {
    var x2 = function x3(d) {
      return d[0];
    }, y2 = function y3(d) {
      return d[1];
    }, domain;
    function linear4(data) {
      var n = 0, X2 = 0, Y2 = 0, XY2 = 0, X22 = 0, xmin = domain ? +domain[0] : Infinity, xmax = domain ? +domain[1] : -Infinity;
      visitPoints(data, x2, y2, function(dx, dy) {
        ++n;
        X2 += (dx - X2) / n;
        Y2 += (dy - Y2) / n;
        XY2 += (dx * dy - XY2) / n;
        X22 += (dx * dx - X22) / n;
        if (!domain) {
          if (dx < xmin)
            xmin = dx;
          if (dx > xmax)
            xmax = dx;
        }
      });
      var _ols = ols(X2, Y2, XY2, X22), _ols2 = _slicedToArray(_ols, 2), intercept = _ols2[0], slope = _ols2[1], fn = function fn2(x3) {
        return slope * x3 + intercept;
      }, out = [[xmin, fn(xmin)], [xmax, fn(xmax)]];
      out.a = slope;
      out.b = intercept;
      out.predict = fn;
      out.rSquared = determination(data, x2, y2, Y2, fn);
      return out;
    }
    linear4.domain = function(arr) {
      return arguments.length ? (domain = arr, linear4) : domain;
    };
    linear4.x = function(fn) {
      return arguments.length ? (x2 = fn, linear4) : x2;
    };
    linear4.y = function(fn) {
      return arguments.length ? (y2 = fn, linear4) : y2;
    };
    return linear4;
  }
  function median(arr) {
    arr.sort(function(a2, b) {
      return a2 - b;
    });
    var i = arr.length / 2;
    return i % 1 === 0 ? (arr[i - 1] + arr[i]) / 2 : arr[Math.floor(i)];
  }
  var maxiters = 2;
  var epsilon3 = 1e-12;
  function loess() {
    var x2 = function x3(d) {
      return d[0];
    }, y2 = function y3(d) {
      return d[1];
    }, bandwidth = 0.3;
    function loess2(data) {
      var _points = points(data, x2, y2, true), _points2 = _slicedToArray(_points, 4), xv = _points2[0], yv = _points2[1], ux = _points2[2], uy = _points2[3], n = xv.length, bw = Math.max(2, ~~(bandwidth * n)), yhat = new Float64Array(n), residuals = new Float64Array(n), robustWeights = new Float64Array(n).fill(1);
      for (var iter = -1; ++iter <= maxiters; ) {
        var interval2 = [0, bw - 1];
        for (var i = 0; i < n; ++i) {
          var dx = xv[i], i0 = interval2[0], i1 = interval2[1], edge = dx - xv[i0] > xv[i1] - dx ? i0 : i1;
          var W = 0, X2 = 0, Y2 = 0, XY2 = 0, X22 = 0, denom = 1 / Math.abs(xv[edge] - dx || 1);
          for (var k = i0; k <= i1; ++k) {
            var xk = xv[k], yk = yv[k], w = tricube(Math.abs(dx - xk) * denom) * robustWeights[k], xkw = xk * w;
            W += w;
            X2 += xkw;
            Y2 += yk * w;
            XY2 += yk * xkw;
            X22 += xk * xkw;
          }
          var _ols = ols(X2 / W, Y2 / W, XY2 / W, X22 / W), _ols2 = _slicedToArray(_ols, 2), a2 = _ols2[0], b = _ols2[1];
          yhat[i] = a2 + b * dx;
          residuals[i] = Math.abs(yv[i] - yhat[i]);
          updateInterval(xv, i + 1, interval2);
        }
        if (iter === maxiters) {
          break;
        }
        var medianResidual = median(residuals);
        if (Math.abs(medianResidual) < epsilon3)
          break;
        for (var _i = 0, arg, _w; _i < n; ++_i) {
          arg = residuals[_i] / (6 * medianResidual);
          robustWeights[_i] = arg >= 1 ? epsilon3 : (_w = 1 - arg * arg) * _w;
        }
      }
      return output(xv, yhat, ux, uy);
    }
    loess2.bandwidth = function(bw) {
      return arguments.length ? (bandwidth = bw, loess2) : bandwidth;
    };
    loess2.x = function(fn) {
      return arguments.length ? (x2 = fn, loess2) : x2;
    };
    loess2.y = function(fn) {
      return arguments.length ? (y2 = fn, loess2) : y2;
    };
    return loess2;
  }
  function tricube(x2) {
    return (x2 = 1 - x2 * x2 * x2) * x2 * x2;
  }
  function updateInterval(xv, i, interval2) {
    var val = xv[i], left2 = interval2[0], right2 = interval2[1] + 1;
    if (right2 >= xv.length)
      return;
    while (i > left2 && xv[right2] - val <= val - xv[left2]) {
      interval2[0] = ++left2;
      interval2[1] = right2;
      ++right2;
    }
  }
  function output(xv, yhat, ux, uy) {
    var n = xv.length, out = [];
    var i = 0, cnt = 0, prev = [], v;
    for (; i < n; ++i) {
      v = xv[i] + ux;
      if (prev[0] === v) {
        prev[1] += (yhat[i] - prev[1]) / ++cnt;
      } else {
        cnt = 0;
        prev[1] += uy;
        prev = [v, yhat[i]];
        out.push(prev);
      }
    }
    prev[1] += uy;
    return out;
  }

  // src/Typescript/regressionData.ts
  var RegressionData = class _RegressionData {
    constructor(regressionPlot, zFilter) {
      this.uncutLinearRegression = null;
      this.cutLinearRegression = null;
      this.uncutLoessRegression = null;
      this.cutLoessRegression = null;
      this.plot = regressionPlot;
      this.zFilter = zFilter;
    }
    static {
      this.bandwidth = 0.75;
    }
    computePlot() {
      const xDim = this.plot.scatterPlot.xColumn.dim;
      const yDim = this.plot.scatterPlot.yColumn.dim;
      const filteredUncutData = this.filterData(this.plot.scatterPlot.spData.sampleData);
      if (this.plot.useLinearRep()) {
        this.uncutLinearRegression = linear3().x((d) => d[xDim]).y((d) => d[yDim])(filteredUncutData);
      }
      if (this.plot.useLoessRep()) {
        this.uncutLoessRegression = loess().x((d) => d[xDim]).y((d) => d[yDim]).bandwidth(_RegressionData.bandwidth)(filteredUncutData);
      }
      const filteredCutData = this.filterData(this.plot.scatterPlot.spData.cutData());
      if (this.plot.useLinearRep()) {
        this.cutLinearRegression = linear3().x((d) => d[xDim]).y((d) => d[yDim])(filteredCutData);
      }
      if (this.plot.useLoessRep()) {
        this.cutLoessRegression = loess().x((d) => d[xDim]).y((d) => d[yDim]).bandwidth(_RegressionData.bandwidth)(filteredCutData);
      }
      return this;
    }
    filterData(data) {
      return this.zFilter ? data.filter(this.zFilter) : data;
    }
  };

  // src/Typescript/regressionPlot.ts
  var RegressionPlot = class _RegressionPlot {
    static {
      this.NO_FILTER = 0;
    }
    static {
      this.CUT_FILTER = 1;
    }
    static {
      this.SUB_FILTER = 1 << 1;
    }
    static {
      this.LINEAR_REP = 1;
    }
    static {
      this.LOESS_REP = 1 << 1;
    }
    constructor(scatterPlot) {
      this.scatterPlot = scatterPlot;
      this.mainRegr = new RegressionData(this);
      this.subRegrList = [];
      this.subColorScale = SpConst.CATEGORIAL_CS[this.scatterPlot.categoricalCsId];
    }
    generate(plotSelection) {
      const regrPlot = plotSelection.append("g").attr("class", "regressionPlots");
      const uncut = regrPlot.append("g").attr("class", "uncut");
      const cut = regrPlot.append("g").attr("class", "cut");
      cut.append("g").attr("class", "linearGroup");
      cut.append("g").attr("class", "subLinearGroup");
      uncut.append("g").attr("class", "linearGroup");
      uncut.append("g").attr("class", "subLinearGroup");
      cut.append("g").attr("class", "loessGroup");
      cut.append("g").attr("class", "subLoessGroup");
      uncut.append("g").attr("class", "loessGroup");
      uncut.append("g").attr("class", "subLoessGroup");
      return this;
    }
    useLinearRep() {
      return (this.scatterPlot.regressionType & _RegressionPlot.LINEAR_REP) !== 0 && this.scatterPlot.xColumn.categories === null && this.scatterPlot.yColumn.categories === null;
    }
    useLoessRep() {
      return (this.scatterPlot.regressionType & _RegressionPlot.LOESS_REP) !== 0 && this.scatterPlot.xColumn.categories === null && this.scatterPlot.yColumn.categories === null;
    }
    colorScale(colorScale) {
      this.subColorScale = colorScale;
      return this;
    }
    update(_updateType, regrGroup) {
      this.updateLinearPlot(regrGroup, _RegressionPlot.SUB_FILTER);
      this.updateLinearPlot(regrGroup, _RegressionPlot.SUB_FILTER | _RegressionPlot.CUT_FILTER);
      this.updateLinearPlot(regrGroup, _RegressionPlot.CUT_FILTER);
      this.updateLinearPlot(regrGroup, _RegressionPlot.NO_FILTER);
      this.updateLoessPlot(regrGroup, _RegressionPlot.SUB_FILTER);
      this.updateLoessPlot(regrGroup, _RegressionPlot.SUB_FILTER | _RegressionPlot.CUT_FILTER);
      this.updateLoessPlot(regrGroup, _RegressionPlot.CUT_FILTER);
      this.updateLoessPlot(regrGroup, _RegressionPlot.NO_FILTER);
    }
    plotVisibility(filtering) {
      if (filtering & _RegressionPlot.CUT_FILTER) {
        if (filtering & _RegressionPlot.SUB_FILTER) {
          return this.subRegrList.length !== 0;
        } else {
          return true;
        }
      } else {
        return false;
      }
    }
    updateLinearPlot(regrGroup, filtering) {
      const thisPlot = this;
      const dataList = filtering & _RegressionPlot.SUB_FILTER ? this.subRegrList : [this.mainRegr];
      const cutClass = filtering & _RegressionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const linearClass = filtering & _RegressionPlot.SUB_FILTER ? ".subLinearGroup" : ".linearGroup";
      const plotGroup = regrGroup.select(`${cutClass} ${linearClass}`);
      if (this.useLinearRep() && this.plotVisibility(filtering)) {
        plotGroup.style("display", null);
      } else {
        plotGroup.style("display", "none");
        return;
      }
      plotGroup.selectAll("line").data(dataList).join(
        (enter) => enter.append("line"),
        (update) => update,
        (exit) => exit.remove()
      ).attr("stroke-width", this.scatterPlot.style.plotProperties.regression.strokeWidth).attr("stroke", (_data, i) => this.strokeColor(i, filtering)).attr("fill", "none").attr("x1", (data) => thisPlot.linearRegressionX1(data, filtering)).attr("x2", (data) => thisPlot.linearRegressionX2(data, filtering)).attr("y1", (data) => thisPlot.linearRegressionY1(data, filtering)).attr("y2", (data) => thisPlot.linearRegressionY2(data, filtering)).on("mouseover", function(_event, regressionData) {
        const i = dataList.indexOf(regressionData);
        thisPlot.mouseoverLinear(this, regressionData, i, filtering);
      }).on("mouseout", function(_event, regressionData) {
        if (thisPlot.scatterPlot.mouseMode === SpConst.tooltipMouse.key) {
          const i = dataList.indexOf(regressionData);
          select_default2(thisPlot.scatterPlot.bindto + " .mspTooltip").style("display", "none");
          select_default2(this).attr("stroke", () => thisPlot.strokeColor(i, filtering));
        }
      });
    }
    strokeColor(catIndex, filtering, k) {
      if (!(filtering & _RegressionPlot.CUT_FILTER)) {
        return null;
      }
      const color2 = filtering & _RegressionPlot.SUB_FILTER ? this.subColorScale(catIndex) : this.scatterPlot.style.plotProperties.noCatColor;
      const d3Color = color(color2);
      return d3Color ? d3Color.darker(k).toString() : color2;
    }
    linearRegressionX1(data, filtering) {
      const linearRegression = _RegressionPlot.linearRegression(data, filtering);
      return this.xScale(linearRegression[0][0]);
    }
    linearRegressionX2(data, filtering) {
      const linearRegression = _RegressionPlot.linearRegression(data, filtering);
      return this.xScale(linearRegression[1][0]);
    }
    linearRegressionY1(data, filtering) {
      const linearRegression = _RegressionPlot.linearRegression(data, filtering);
      return this.yScale(linearRegression[0][1]);
    }
    linearRegressionY2(data, filtering) {
      const linearRegression = _RegressionPlot.linearRegression(data, filtering);
      return this.yScale(linearRegression[1][1]);
    }
    mouseoverLinear(line, data, i, filtering) {
      if (this.scatterPlot.mouseMode !== SpConst.tooltipMouse.key) {
        return;
      }
      select_default2(line).attr("stroke", () => this.strokeColor(i, filtering, 2));
      const rSquared = data.cutLinearRegression ? ExpFormat.format(data.cutLinearRegression.rSquared) : "Not computed";
      const tooltipLocation = this.tooltipLocation();
      select_default2(this.scatterPlot.bindto + " .mspTooltip").remove();
      const mspDiv = select_default2(this.scatterPlot.bindto + " .mspDiv");
      const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
      tooltip.append("div").attr("class", "title").html("Linear Regression");
      if (filtering & _RegressionPlot.SUB_FILTER && this.scatterPlot.zColumn !== null) {
        const coloringHtml = `<span class='swatch' style='background:${this.strokeColor(i, filtering)}'>&nbsp;</span>`;
        const category = this.scatterPlot.zColumn.categories ? this.scatterPlot.zColumn.categories[i] : "undefined";
        tooltip.append("div").html(`where ${this.scatterPlot.zColumn.labelText()} is: ${_RegressionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
      }
      if (filtering & _RegressionPlot.CUT_FILTER) {
        const filteredUncutData = data.filterData(this.scatterPlot.spData.sampleData);
        const filteredCutData = data.filterData(this.scatterPlot.spData.cutData());
        tooltip.append("div").html(`${filteredCutData.length} sample points (${filteredUncutData.length - filteredCutData.length} filtered points)`);
      }
      tooltip.append("div").html(`<span class="r2">=> r2</span>: <span class="r2Value">${rSquared}</span>`);
    }
    static brIze(category) {
      const splittedCategory = category.toString().split(", ");
      if (splittedCategory.length > 1) {
        return "<br>" + splittedCategory.join(",<br>") + "<br>";
      }
      return splittedCategory[0];
    }
    tooltipLocation() {
      const mspDivNode = select_default2(this.scatterPlot.bindto + " .mspDiv").node();
      const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
      const xParent = parentBounds === null ? 0 : parentBounds.x;
      const plotGroup = select_default2(this.scatterPlot.bindto + " .mspGroup").node();
      const elementBounds = plotGroup === null ? null : plotGroup.getBoundingClientRect();
      const xRect = elementBounds === null ? 0 : elementBounds.x;
      const wRect = elementBounds === null ? 0 : elementBounds.width;
      return [xRect - xParent + wRect + 5, this.scatterPlot.yPlot];
    }
    updateLoessPlot(regrGroup, filtering) {
      const thisPlot = this;
      const dataList = filtering & _RegressionPlot.SUB_FILTER ? this.subRegrList : [this.mainRegr];
      const cutClass = filtering & _RegressionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const loessClass = filtering & _RegressionPlot.SUB_FILTER ? ".subLoessGroup" : ".loessGroup";
      const plotGroup = regrGroup.select(`${cutClass} ${loessClass}`);
      if (this.useLoessRep() && this.plotVisibility(filtering)) {
        plotGroup.style("display", null);
      } else {
        plotGroup.style("display", "none");
        return;
      }
      plotGroup.selectAll("path").data(dataList).join(
        (enter) => enter.append("path"),
        (update) => update,
        (exit) => exit.remove()
      ).attr("stroke-width", this.scatterPlot.style.plotProperties.regression.strokeWidth).attr("stroke", (_data, i) => this.strokeColor(i, filtering)).attr("fill", "none").attr("d", function(data) {
        const loessRegression = _RegressionPlot.loessRegression(data, filtering);
        const lineGenerator = line_default().curve(basis_default2).x((d) => thisPlot.xScale(d[0])).y((d) => thisPlot.yScale(d[1]));
        return lineGenerator(loessRegression);
      }).on("mouseover", function(_event, regressionData) {
        const i = dataList.indexOf(regressionData);
        thisPlot.mouseoverLoess(this, regressionData, i, filtering);
      }).on("mouseout", function(_event, regressionData) {
        if (thisPlot.scatterPlot.mouseMode === SpConst.tooltipMouse.key) {
          const i = dataList.indexOf(regressionData);
          select_default2(thisPlot.scatterPlot.bindto + " .mspTooltip").style("display", "none");
          select_default2(this).attr("stroke", () => thisPlot.strokeColor(i, filtering));
        }
      });
    }
    xScale(value) {
      const scaled = this.scatterPlot.xScale(value);
      return typeof scaled === "undefined" ? NaN : scaled;
    }
    yScale(value) {
      const scaled = this.scatterPlot.yScale(value);
      return typeof scaled === "undefined" ? NaN : scaled;
    }
    mouseoverLoess(path2, data, i, filtering) {
      if (this.scatterPlot.mouseMode !== SpConst.tooltipMouse.key) {
        return;
      }
      select_default2(path2).attr("stroke", () => this.strokeColor(i, filtering, 2));
      const tooltipLocation = this.tooltipLocation();
      select_default2(this.scatterPlot.bindto + " .mspTooltip").remove();
      const mspDiv = select_default2(this.scatterPlot.bindto + " .mspDiv");
      const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
      tooltip.append("div").attr("class", "title").html("Local Polynomial Regression");
      if (filtering & _RegressionPlot.SUB_FILTER && this.scatterPlot.zColumn !== null) {
        const category = this.scatterPlot.zColumn.categories ? this.scatterPlot.zColumn.categories[i] : "undefined";
        const coloringHtml = `<span class='swatch' style='background:${this.strokeColor(i, filtering)}'>&nbsp;</span>`;
        tooltip.append("div").html(`where ${this.scatterPlot.zColumn.labelText()} is: ${_RegressionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
      }
      if (filtering & _RegressionPlot.CUT_FILTER) {
        const filteredUncutData = data.filterData(this.scatterPlot.spData.sampleData);
        const filteredCutData = data.filterData(this.scatterPlot.spData.cutData());
        tooltip.append("div").html(`${filteredCutData.length} sample points (${filteredUncutData.length - filteredCutData.length} filtered points)`);
      }
    }
    static linearRegression(data, filtering) {
      let linearRegression = filtering & _RegressionPlot.CUT_FILTER ? data.cutLinearRegression : data.uncutLinearRegression;
      if (!linearRegression) {
        console.error("RegressionPlot.update called but no linear regression computed");
        linearRegression = [[0, 0]];
        linearRegression.rSquared = 0;
      }
      return linearRegression;
    }
    static loessRegression(data, filtering) {
      let loessRegression = filtering & _RegressionPlot.CUT_FILTER ? data.cutLoessRegression : data.uncutLoessRegression;
      if (!loessRegression) {
        console.error("RegressionPlot.update called but no loess regression computed");
        loessRegression = [[0, 0]];
      }
      return loessRegression;
    }
    computePlot(zColumn) {
      this.mainRegr.computePlot();
      if (!zColumn || !zColumn.categories) {
        this.subRegrList = [];
      } else {
        this.subRegrList = zColumn.categories.map((_cat, i) => {
          return new RegressionData(this, _RegressionPlot.catFilter(zColumn, i));
        });
      }
      this.subRegrList.forEach((data) => {
        data.computePlot();
      });
      return this;
    }
    static catFilter(column, catIndex) {
      return function(row) {
        return row[column.dim] === catIndex;
      };
    }
  };

  // src/Typescript/distributionPlot.ts
  var DistributionPlot = class _DistributionPlot {
    constructor(spData, column, dpConfig, plotIndex, xPlot, yPlot) {
      this.valuesScale = linear2();
      this.zColumn = null;
      this.xPlot = 0;
      this.yPlot = 0;
      this.plotIndex = 0;
      this.bindto = dpConfig.bindto;
      this.spData = spData;
      this.mouseMode = dpConfig.mouseMode;
      this.distribType = dpConfig.distribType;
      this.column = column;
      this.orientation = dpConfig.orientation;
      this.style = dpConfig.style;
      this.plotIndex = plotIndex;
      this.xPlot = xPlot;
      this.yPlot = yPlot;
      this.valuesScale.domain(this.column.domain());
      if (this.column.categories === null) {
        this.valuesScale.nice();
      }
      this.mainDistrib = new DistributionData(this);
      this.subDistribList = [];
      this.subColorScale = SpConst.CATEGORIAL_CS[dpConfig.categoricalCsId];
    }
    static {
      this.HOR = "Horizontal";
    }
    static {
      this.VER = "Vertical";
    }
    static {
      this.NO_FILTER = 0;
    }
    static {
      this.CUT_FILTER = 1;
    }
    static {
      this.SUB_FILTER = 1 << 1;
    }
    static {
      this.DENS_REP = 1;
    }
    static {
      this.HISTO_REP = 1 << 1;
    }
    generate(distribGroup, clipSelector) {
      const plotClass = this.orientation === _DistributionPlot.HOR ? "hor distribPlot" : "ver distribPlot";
      const distribPlot = distribGroup.append("g").attr("class", plotClass);
      if (clipSelector) {
        distribPlot.attr("clip-path", "url(" + clipSelector + ")");
      }
      const uncut = distribPlot.append("g").attr("class", "uncut");
      const cut = distribPlot.append("g").attr("class", "cut");
      cut.append("g").attr("class", "pdfGroup");
      cut.append("g").attr("class", "subPdfGroup");
      uncut.append("g").attr("class", "pdfGroup");
      uncut.append("g").attr("class", "subPdfGroup");
      cut.append("g").attr("class", "histoGroup");
      cut.append("g").attr("class", "subHistoGroup");
      uncut.append("g").attr("class", "histoGroup");
      uncut.append("g").attr("class", "subHistoGroup");
      return this;
    }
    useDensityRep() {
      return (this.distribType & _DistributionPlot.DENS_REP) !== 0 && this.column.categories === null;
    }
    useHistogramRep() {
      return (this.distribType & _DistributionPlot.HISTO_REP) !== 0 || this.column.categories !== null;
    }
    colorScale(colorScale) {
      this.subColorScale = colorScale;
      return this;
    }
    update(updateType, distribGroup) {
      if (this.orientation === _DistributionPlot.HOR) {
        this.updateHor(updateType, distribGroup);
      } else {
        this.updateVer(updateType, distribGroup);
      }
    }
    updateVer(_updateType, distribGroup) {
      this.updateVerDensityPlot(distribGroup, _DistributionPlot.SUB_FILTER);
      this.updateVerDensityPlot(distribGroup, _DistributionPlot.SUB_FILTER | _DistributionPlot.CUT_FILTER);
      this.updateVerDensityPlot(distribGroup, _DistributionPlot.CUT_FILTER);
      this.updateVerDensityPlot(distribGroup, _DistributionPlot.NO_FILTER);
      this.updateVerHistogram(distribGroup, _DistributionPlot.SUB_FILTER);
      this.updateVerHistogram(distribGroup, _DistributionPlot.SUB_FILTER | _DistributionPlot.CUT_FILTER);
      this.updateVerHistogram(distribGroup, _DistributionPlot.CUT_FILTER);
      this.updateVerHistogram(distribGroup, _DistributionPlot.NO_FILTER);
    }
    updateHor(_updateType, distribGroup) {
      this.updateHorDensityPlot(distribGroup, _DistributionPlot.SUB_FILTER);
      this.updateHorDensityPlot(distribGroup, _DistributionPlot.SUB_FILTER | _DistributionPlot.CUT_FILTER);
      this.updateHorDensityPlot(distribGroup, _DistributionPlot.CUT_FILTER);
      this.updateHorDensityPlot(distribGroup, _DistributionPlot.NO_FILTER);
      this.updateHorHistogram(distribGroup, _DistributionPlot.SUB_FILTER);
      this.updateHorHistogram(distribGroup, _DistributionPlot.SUB_FILTER | _DistributionPlot.CUT_FILTER);
      this.updateHorHistogram(distribGroup, _DistributionPlot.CUT_FILTER);
      this.updateHorHistogram(distribGroup, _DistributionPlot.NO_FILTER);
    }
    plotVisibility(repType, filtering) {
      if (filtering & _DistributionPlot.CUT_FILTER) {
        if (filtering & _DistributionPlot.SUB_FILTER) {
          return this.subDistribList.length !== 0;
        } else {
          return this.subDistribList.length === 0;
        }
      } else {
        if (filtering & _DistributionPlot.SUB_FILTER) {
          return false;
        } else {
          if (repType & _DistributionPlot.HISTO_REP) {
            return _DistributionPlot.equalsDomain(this.mainDistrib.cutHistoScale.domain(), this.mainDistrib.uncutHistoScale.domain());
          } else {
            return true;
          }
        }
      }
    }
    updateHorDensityPlot(distribGroup, filtering) {
      const thisPlot = this;
      const dataList = filtering & _DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
      const cutClass = filtering & _DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const pdfClass = filtering & _DistributionPlot.SUB_FILTER ? ".subPdfGroup" : ".pdfGroup";
      const plotGroup = distribGroup.select(`.hor ${cutClass} ${pdfClass}`);
      if (this.useDensityRep() && this.plotVisibility(_DistributionPlot.DENS_REP, filtering)) {
        plotGroup.style("display", null);
      } else {
        plotGroup.style("display", "none");
        return;
      }
      const densPath = plotGroup.selectAll("path").data(dataList).join(
        (enter) => enter.append("path"),
        (update) => update,
        (exit) => exit.remove()
      ).attr("fill", (_data, dataIndex) => this.fillColor(dataIndex, filtering)).attr("d", function(data) {
        let density = filtering & _DistributionPlot.CUT_FILTER ? data.cutDensity : data.uncutDensity;
        if (!density) {
          console.error("DistributionPlot.update called but no density computed");
          density = [[0, 0]];
        }
        const densityScale = filtering & _DistributionPlot.CUT_FILTER ? data.cutDensityScale : data.uncutDensityScale;
        const lineGenerator = line_default().curve(natural_default).x((d) => {
          const x2 = thisPlot.valuesScale(d[0]);
          return _DistributionPlot.undef2Nan(x2);
        }).y((d) => {
          const y2 = densityScale(d[1]);
          return _DistributionPlot.undef2Nan(y2);
        });
        return lineGenerator(density);
      });
      thisPlot.densMouse(densPath, filtering);
    }
    updateVerDensityPlot(distribGroup, filtering) {
      const thisPlot = this;
      const dataList = filtering & _DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
      const cutClass = filtering & _DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const pdfClass = filtering & _DistributionPlot.SUB_FILTER ? ".subPdfGroup" : ".pdfGroup";
      const plotGroup = distribGroup.select(`.ver ${cutClass} ${pdfClass}`);
      if (this.useDensityRep() && this.plotVisibility(_DistributionPlot.DENS_REP, filtering)) {
        plotGroup.style("display", null);
      } else {
        plotGroup.style("display", "none");
        return;
      }
      const densPath = plotGroup.selectAll("path").data(dataList).join(
        (enter) => enter.append("path"),
        (update) => update,
        (exit) => exit.remove()
      ).attr("fill", (_data, dataIndex) => this.fillColor(dataIndex, filtering)).attr("d", function(data) {
        let density = filtering & _DistributionPlot.CUT_FILTER ? data.cutDensity : data.uncutDensity;
        if (!density) {
          console.error("DistributionPlot.update called but no density computed");
          density = [[0, 0]];
        }
        const densityScale = filtering & _DistributionPlot.CUT_FILTER ? data.cutDensityScale : data.uncutDensityScale;
        const lineGenerator = line_default().curve(basis_default2).x((d) => {
          const x2 = densityScale(d[1]);
          return _DistributionPlot.undef2Nan(x2);
        }).y((d) => {
          const y2 = thisPlot.valuesScale(d[0]);
          return _DistributionPlot.undef2Nan(y2);
        });
        return lineGenerator(density);
      });
      thisPlot.densMouse(densPath, filtering);
    }
    // eslint-disable-next-line max-lines-per-function
    densMouse(densPath, filtering) {
      if (filtering & _DistributionPlot.CUT_FILTER) {
        const thisPlot = this;
        densPath.on("mouseover", function(_event, data) {
          if (thisPlot.mouseMode !== SpConst.tooltipMouse.key) {
            return;
          }
          const dataIndex = densPath.nodes().indexOf(this);
          select_default2(this).attr("fill", thisPlot.fillColor(dataIndex, filtering, 3));
          thisPlot.pdfDisplay(filtering, dataIndex, false);
          const tooltipLocation = thisPlot.tooltipLocation();
          select_default2(thisPlot.bindto + " .mspTooltip").remove();
          const mspDiv = select_default2(thisPlot.bindto + " .mspDiv");
          const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
          tooltip.append("div").attr("class", "title").html("Density Curve");
          tooltip.append("div").html(`<span class="pdfColumn">Marginal distribution for</span>: <span class="pdfColumnLabel">${thisPlot.column.labelText()}`);
          if (data.violinCatDescription) {
            const category = data.violinCatDescription.column.categories ? data.violinCatDescription.column.categories[data.violinCatDescription.catIndex] : "undefined";
            const axisName = thisPlot.orientation === _DistributionPlot.HOR ? "y axis" : "x axis";
            tooltip.append("div").html(`conditional on ${data.violinCatDescription.column.labelText()} = ${category} (${axisName} )`);
          }
          if (filtering & RegressionPlot.SUB_FILTER && thisPlot.column !== thisPlot.zColumn) {
            if (thisPlot.zColumn) {
              const category = thisPlot.zColumn.categories ? thisPlot.zColumn.categories[dataIndex] : "undefined";
              const coloringHtml = `<span class='swatch' style='background:${thisPlot.fillColor(dataIndex, filtering)}'>&nbsp;</span>`;
              if (data.violinCatDescription) {
                tooltip.append("div").html(`and on ${thisPlot.zColumn.labelText()} = ${_DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
              } else {
                tooltip.append("div").html(`conditional on ${thisPlot.zColumn.labelText()} = ${_DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
              }
            } else {
              tooltip.append("div").html("conditional on undefined = undefined");
            }
          }
          if (filtering & RegressionPlot.CUT_FILTER) {
            const filteredUncutData = data.filterData(thisPlot.spData.sampleData);
            const filteredCutData = data.filterData(thisPlot.spData.cutData());
            tooltip.append("div").html(`${filteredCutData.length} sample points (filtered points: ${filteredUncutData.length - filteredCutData.length})`);
          }
        }).on("mouseout", function() {
          if (thisPlot.mouseMode === SpConst.tooltipMouse.key) {
            const dataIndex = densPath.nodes().indexOf(this);
            select_default2(this).attr("fill", thisPlot.fillColor(dataIndex, filtering));
            thisPlot.pdfDisplay(filtering, dataIndex, true);
            select_default2(thisPlot.bindto + " .mspTooltip").style("display", "none");
          }
        });
      } else {
        densPath.style("pointer-events", "none");
      }
    }
    tooltipLocation() {
      const mspDivNode = select_default2(this.bindto + " .mspDiv").node();
      const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
      const xParent = parentBounds === null ? 0 : parentBounds.x;
      const plotGroup = select_default2(this.bindto + " .mspGroup").node();
      const elementBounds = plotGroup === null ? null : plotGroup.getBoundingClientRect();
      const xRect = elementBounds === null ? 0 : elementBounds.x;
      const wRect = elementBounds === null ? 0 : elementBounds.width;
      return [xRect - xParent + wRect + 5, this.yPlot];
    }
    pdfDisplay(filtering, dataIndex, display) {
      const mspGroup = select_default2(this.bindto + " .mspGroup");
      const cutClass = filtering & _DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const pdfGroup = mspGroup.selectAll(`${cutClass} .pdfGroup`);
      const pdfPath = pdfGroup.selectAll("path").filter(function(_data, dataIndex2) {
        return dataIndex2 !== dataIndex;
      });
      const subPdfGroup = mspGroup.selectAll(`${cutClass} .subPdfGroup`);
      const subPdfPath = subPdfGroup.selectAll("path").filter(function(_data, dataIndex2) {
        return dataIndex2 !== dataIndex;
      });
      if (display) {
        pdfPath.style("display", null);
        subPdfPath.style("display", null);
      } else {
        pdfPath.style("display", "none");
        subPdfPath.style("display", "none");
      }
    }
    // eslint-disable-next-line max-lines-per-function
    updateHorHistogram(distribGroup, filtering) {
      const thisPlot = this;
      const dataList = filtering & _DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
      const cutClass = filtering & _DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const histoClass = filtering & _DistributionPlot.SUB_FILTER ? ".subHistoGroup" : ".histoGroup";
      const plotGroup = distribGroup.select(`.hor ${cutClass} ${histoClass}`);
      if (this.useHistogramRep() && this.plotVisibility(_DistributionPlot.HISTO_REP, filtering)) {
        plotGroup.style("display", null);
      } else {
        plotGroup.style("display", "none");
        return;
      }
      const binsGroup = plotGroup.selectAll("g").data(dataList).join(
        (enter) => enter.append("g"),
        (update) => update,
        (exit) => exit.remove()
      );
      let baseLineList = [];
      binsGroup.each(function(data, dataIndex) {
        let bins = filtering & _DistributionPlot.CUT_FILTER ? data.cutBins : data.uncutBins;
        if (!bins) {
          console.error("DistribtionPlot.update called but no histogram computed");
          bins = [];
        }
        if (dataIndex === 0) {
          baseLineList = bins.map((_bin) => 0);
        }
        const histoScale = filtering & _DistributionPlot.CUT_FILTER ? data.cutHistoScale : data.uncutHistoScale;
        const binsSelection = select_default2(this);
        binsSelection.selectAll("rect").remove();
        const binRect = binsSelection.selectAll("rect").data(bins).enter().append("rect").attr("fill", thisPlot.fillColor(dataIndex, filtering)).attr("x", function(bin2) {
          if (typeof bin2.x0 === "undefined") {
            console.error("bin.x0 is undefined");
            return NaN;
          }
          const x2 = thisPlot.valuesScale(bin2.x0);
          return _DistributionPlot.undef2Nan(x2);
        }).attr("y", function(bin2, binIndex) {
          const y2 = histoScale(bin2.length + baseLineList[binIndex]);
          return _DistributionPlot.undef2Nan(y2);
        }).attr("width", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x0 or bin.x1 are undefined");
            return NaN;
          }
          const x0 = _DistributionPlot.undef2Nan(thisPlot.valuesScale(bin2.x0));
          const x1 = _DistributionPlot.undef2Nan(thisPlot.valuesScale(bin2.x1));
          return x1 - x0;
        }).attr("height", function(bin2) {
          const y0 = _DistributionPlot.undef2Nan(histoScale(0));
          const y1 = _DistributionPlot.undef2Nan(histoScale(bin2.length));
          return y0 - y1;
        });
        thisPlot.histoMouse(binRect, filtering, data, dataIndex);
        bins.forEach((bin2, binIndex) => {
          baseLineList[binIndex] += bin2.length;
        });
      });
    }
    // eslint-disable-next-line max-lines-per-function
    updateVerHistogram(distribGroup, filtering) {
      const thisPlot = this;
      const dataList = filtering & _DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
      const cutClass = filtering & _DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
      const histoClass = filtering & _DistributionPlot.SUB_FILTER ? ".subHistoGroup" : ".histoGroup";
      const plotGroup = distribGroup.select(`.ver ${cutClass} ${histoClass}`);
      if (this.useHistogramRep() && this.plotVisibility(_DistributionPlot.HISTO_REP, filtering)) {
        plotGroup.style("display", null);
      } else {
        plotGroup.style("display", "none");
        return;
      }
      const binsGroup = plotGroup.selectAll("g").data(dataList).join(
        (enter) => enter.append("g"),
        (update) => update,
        (exit) => exit.remove()
      );
      let baseLineList = [];
      binsGroup.each(function(data, dataIndex) {
        let bins = filtering & _DistributionPlot.CUT_FILTER ? data.cutBins : data.uncutBins;
        if (!bins) {
          console.error("DistribtionPlot.update called but no histogram computed");
          bins = [];
        }
        if (dataIndex === 0) {
          baseLineList = bins.map((_bin) => 0);
        }
        const histoScale = filtering & _DistributionPlot.CUT_FILTER ? data.cutHistoScale : data.uncutHistoScale;
        const binsSelection = select_default2(this);
        binsSelection.selectAll("rect").remove();
        const binRect = binsSelection.selectAll("rect").data(bins).enter().append("rect").attr("fill", thisPlot.fillColor(dataIndex, filtering)).attr("y", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x0 or bin.x1 are undefined");
            return NaN;
          }
          const y0 = _DistributionPlot.undef2Nan(thisPlot.valuesScale(bin2.x0));
          const y1 = _DistributionPlot.undef2Nan(thisPlot.valuesScale(bin2.x1));
          return Math.min(y0, y1);
        }).attr("x", function(_bin, binIndex) {
          const x2 = histoScale(0 + baseLineList[binIndex]);
          return _DistributionPlot.undef2Nan(x2);
        }).attr("height", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x0 or bin.x1 are undefined");
            return NaN;
          }
          const y1 = _DistributionPlot.undef2Nan(thisPlot.valuesScale(bin2.x1));
          const y0 = _DistributionPlot.undef2Nan(thisPlot.valuesScale(bin2.x0));
          return Math.abs(y1 - y0);
        }).attr("width", function(bin2) {
          const x0 = _DistributionPlot.undef2Nan(histoScale(0));
          const x1 = _DistributionPlot.undef2Nan(histoScale(bin2.length));
          return x1 - x0;
        });
        thisPlot.histoMouse(binRect, filtering, data, dataIndex);
        bins.forEach((bin2, binIndex) => {
          baseLineList[binIndex] += bin2.length;
        });
      });
    }
    static undef2Nan(value) {
      return typeof value === "undefined" ? NaN : value;
    }
    // eslint-disable-next-line max-lines-per-function
    histoMouse(binRect, filtering, data, dataIndex) {
      if (filtering & _DistributionPlot.CUT_FILTER) {
        const thisPlot = this;
        binRect.on("mouseover", function(_event, bin2) {
          if (thisPlot.mouseMode !== SpConst.tooltipMouse.key) {
            return;
          }
          select_default2(this).attr("fill", thisPlot.fillColor(dataIndex, filtering, 3));
          const x0 = typeof bin2.x0 === "undefined" ? "undefined" : ExpFormat.format(bin2.x0);
          const x1 = typeof bin2.x1 === "undefined" ? "undefined" : ExpFormat.format(bin2.x1);
          const tooltipLocation = thisPlot.tooltipLocation();
          select_default2(thisPlot.bindto + " .mspTooltip").remove();
          const mspDiv = select_default2(thisPlot.bindto + " .mspDiv");
          const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
          tooltip.append("div").attr("class", "title").html("Histogram");
          if (thisPlot.column.categories) {
            if (typeof bin2.x0 !== "undefined" && typeof bin2.x1 !== "undefined") {
              const cat = thisPlot.column.categories[(bin2.x0 + bin2.x1) / 2];
              tooltip.append("div").html(`<span class="binColumn">bin for</span>: <span class="binColumnLabel">${thisPlot.column.labelText()}</span> = <span class="domainValue">${cat}</span>`);
            }
          } else {
            tooltip.append("div").html(`<span class="binColumn">bin for</span>: <span class="binColumnValue">${thisPlot.column.labelText()}</span> \u2208 <span class="domainValue">[${x0}, ${x1}[</span> `);
          }
          if (data.violinCatDescription) {
            const category = data.violinCatDescription.column.categories ? data.violinCatDescription.column.categories[data.violinCatDescription.catIndex] : "undefined";
            const axisName = thisPlot.orientation === _DistributionPlot.HOR ? "y axis" : "x axis";
            tooltip.append("div").html(`where ${data.violinCatDescription.column.labelText()} = ${category} (${axisName})`);
          }
          if (filtering & RegressionPlot.SUB_FILTER && thisPlot.column !== thisPlot.zColumn) {
            if (thisPlot.zColumn) {
              const category = thisPlot.zColumn.categories ? thisPlot.zColumn.categories[dataIndex] : "undefined";
              const coloringHtml = `<span class='swatch' style='background:${thisPlot.fillColor(dataIndex, filtering)}'>&nbsp;</span>`;
              if (data.violinCatDescription) {
                tooltip.append("div").html(`and ${thisPlot.zColumn.labelText()} is: ${_DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
              } else {
                tooltip.append("div").html(`where ${thisPlot.zColumn.labelText()} is: ${_DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
              }
            } else {
              tooltip.append("div").html("where zaxis undefined = undefined");
            }
          }
          const binIndex = binRect.nodes().indexOf(this);
          const uncutBins = data.uncutBins;
          const cutInfo = uncutBins ? `(${uncutBins[binIndex].length - bin2.length} filtered points)` : "";
          tooltip.append("div").html(`<span class="binLength">=> <span class="binLengthValue">${bin2.length}</span> sample points</span> ${cutInfo}`);
        }).on("mouseout", function() {
          if (thisPlot.mouseMode === SpConst.tooltipMouse.key) {
            select_default2(this).attr("fill", thisPlot.fillColor(dataIndex, filtering));
            select_default2(thisPlot.bindto + " .mspTooltip").style("display", "none");
          }
        });
      } else {
        binRect.style("pointer-events", "none");
      }
    }
    static brIze(category) {
      const splittedCategory = category.toString().split(", ");
      if (splittedCategory.length > 1) {
        return "<br>" + splittedCategory.join(",<br>") + "<br>";
      }
      return splittedCategory[0];
    }
    fillColor(dataIndex, filtering, k) {
      if (!(filtering & RegressionPlot.CUT_FILTER)) {
        return this.style.plotProperties.watermarkColor;
      }
      const color2 = filtering & RegressionPlot.SUB_FILTER ? this.subColorScale(dataIndex) : this.style.plotProperties.noCatColor;
      const d3Color = color(color2);
      return d3Color ? d3Color.darker(k).toString() : color2;
    }
    computePlot(zColumn, violinCatDescription) {
      this.zColumn = zColumn;
      this.mainDistrib.computePlot(violinCatDescription);
      if (!zColumn || !zColumn.categories) {
        this.subDistribList = [];
      } else {
        this.subDistribList = zColumn.categories.map((_cat, i) => {
          const zCatDescription = { column: zColumn, catIndex: i };
          return new DistributionData(this, zCatDescription);
        });
      }
      this.subDistribList.forEach((data) => {
        data.computePlot(violinCatDescription);
        if (data.plot.useHistogramRep()) {
          data.cutHistoScale.domain(this.mainDistrib.cutHistoScale.domain());
          data.uncutHistoScale.domain(this.mainDistrib.uncutHistoScale.domain());
        }
      });
      return this;
    }
    distribScaleRange(range2) {
      this.mainDistrib.distribScaleRange(range2);
      this.subDistribList.forEach((data) => data.distribScaleRange(range2));
      return this;
    }
    valuesScaleRange(range2) {
      this.valuesScale.range(range2);
      return this;
    }
    static equalsDomain(domain1, domain2) {
      return domain1[0] === domain2[0] && domain1[1] === domain2[1];
    }
  };

  // src/Typescript/multiBrush.ts
  var MultiBrush = class _MultiBrush {
    constructor(plot) {
      // Keep the actual d3-brush functions and their IDs in a list
      this.brushDefList = [];
      this.plot = plot;
      this.addNewBrushDef();
      this.applyDataJoin();
    }
    static multiBrushClass(plotIndex) {
      return "multibrush_plot" + plotIndex;
    }
    static brushClass(plotIndex, brushDef) {
      return "brush" + brushDef.id + "_plot" + plotIndex;
    }
    brushClass(brushDef) {
      return _MultiBrush.brushClass(this.plot.index, brushDef);
    }
    addNewBrushDef(initialXYCutoff) {
      const thisMB = this;
      const tlCorner = [
        this.plot.xScale.range()[0],
        this.plot.yScale.range()[1]
      ];
      const brCorner = [
        this.plot.xScale.range()[1],
        this.plot.yScale.range()[0]
      ];
      const brush2 = brush_default().handleSize(4).extent([tlCorner, brCorner]).on("brush", function() {
        thisMB.updatePlotCutoffs();
        thisMB.plot.spData.sendCutoffEvent(true);
      }).on("end", function() {
        thisMB.updatePlotCutoffs();
        thisMB.updateBrushDefList();
        thisMB.plot.spData.sendCutoffEvent(false);
      });
      const newBrushDef = {
        id: this.brushDefList.length,
        brush: brush2,
        initialXYCutoff
      };
      this.brushDefList.push(newBrushDef);
      return newBrushDef;
    }
    // eslint-disable-next-line max-lines-per-function
    applyDataJoin() {
      const thisMB = this;
      const brushGroup = select_default2(thisMB.plot.bindto + " ." + _MultiBrush.multiBrushClass(this.plot.index)).selectAll(".brush").data(this.brushDefList, (brushDef) => brushDef.id.toString());
      brushGroup.enter().insert("g", ".brush").attr("class", function(brushDef) {
        return ["brush", thisMB.brushClass(brushDef)].join(" ");
      }).each(function(brushDef, brushIndex) {
        select_default2(this).call(brushDef.brush);
        if (brushDef.initialXYCutoff) {
          if (brushDef.initialXYCutoff[0] && brushDef.initialXYCutoff[1]) {
            const tlCorner = [
              thisMB.plot.xScale(brushDef.initialXYCutoff[0][0]),
              thisMB.plot.yScale(brushDef.initialXYCutoff[1][1])
            ];
            const brCorner = [
              thisMB.plot.xScale(brushDef.initialXYCutoff[0][1]),
              thisMB.plot.yScale(brushDef.initialXYCutoff[1][0])
            ];
            select_default2(this).call(brush_default().move, [tlCorner, brCorner]);
          } else {
            console.error(`Plot (${thisMB.plot.xColumn.dim}, ${thisMB.plot.yColumn.dim}), brush ${brushIndex}, unexpected initialXYCutoff: ${brushDef.initialXYCutoff}`);
          }
        }
      });
      brushGroup.each(function(brushDef) {
        select_default2(this).selectAll(".overlay").style("pointer-events", function() {
          return brushDef.id === thisMB.brushDefList.length - 1 && brushDef.brush !== void 0 ? "all" : "none";
        }).on("click", function() {
          thisMB.removeBrushes();
        });
      });
      brushGroup.exit().remove();
    }
    removeBrushes() {
      const brushSelections = [];
      this.plot.setContCutoff(brushSelections);
      this.brushDefList = [];
      this.applyDataJoin();
      this.addNewBrushDef();
      this.applyDataJoin();
      this.plot.spData.sendCutoffEvent(false);
    }
    initFrom(xyCutoffs) {
      const thisMB = this;
      thisMB.brushDefList = [];
      thisMB.applyDataJoin();
      if (xyCutoffs !== null) {
        xyCutoffs.forEach((xyCutoff) => {
          thisMB.addNewBrushDef(xyCutoff);
          thisMB.applyDataJoin();
        });
      }
      thisMB.addNewBrushDef();
      thisMB.applyDataJoin();
    }
    updatePlotCutoffs() {
      const thisMB = this;
      const brushSelections = [];
      this.brushDefList.forEach((brushDef) => {
        const brushGroup = select_default2(thisMB.plot.bindto + " ." + this.brushClass(brushDef));
        const brushSelection2 = brushSelection(
          brushGroup.node()
        );
        if (brushSelection2 !== null) {
          brushSelections.push(brushSelection2);
        }
      });
      this.plot.setContCutoff(brushSelections);
    }
    updateBrushDefList() {
      const lastBrushDef = this.brushDefList[this.brushDefList.length - 1];
      const lastBrushGroup = select_default2(this.plot.bindto + " ." + this.brushClass(lastBrushDef));
      const lastBrushSelection = brushSelection(
        lastBrushGroup.node()
      );
      if (lastBrushSelection && lastBrushSelection[0] !== lastBrushSelection[1]) {
        this.addNewBrushDef();
      }
      this.applyDataJoin();
    }
  };

  // src/Typescript/pointsPlot.ts
  var PointsPlot = class _PointsPlot {
    constructor(scatterPlot) {
      this.scatterPlot = scatterPlot;
    }
    // eslint-disable-next-line max-lines-per-function
    mouseover(row, i, scatterPlotList) {
      if (this.scatterPlot.mouseMode !== SpConst.tooltipMouse.key) {
        return;
      }
      const tooltipTitle = this.scatterPlot.spData.rowLabels ? this.scatterPlot.spData.rowLabels[i] : "Point " + (i + 1);
      const tooltipLocation = this.tooltipLocation();
      select_default2(this.scatterPlot.bindto + " .mspTooltip").remove();
      const mspDiv = select_default2(this.scatterPlot.bindto + " .mspDiv");
      const tooltip = mspDiv.append("div").attr("class", "mspTooltip").style("display", "block").style("left", tooltipLocation[0] + "px").style("top", tooltipLocation[1] + "px");
      tooltip.append("div").attr("class", "pointIndex title").html(tooltipTitle);
      const xDimSet = new Set(scatterPlotList.map((sp) => sp.xColumn.dim));
      const yDimSet = new Set(scatterPlotList.map((sp) => sp.yColumn.dim));
      const dimsToPrint = this.scatterPlot.spData.dimensions.filter((dim) => xDimSet.has(dim) || yDimSet.has(dim));
      if (this.scatterPlot.zColumn !== null && this.scatterPlot.zColumn.categories && !dimsToPrint.includes(this.scatterPlot.zColumn.dim)) {
        dimsToPrint.push(this.scatterPlot.zColumn.dim);
      }
      dimsToPrint.forEach((dim) => {
        const column = this.scatterPlot.spData.columns[dim];
        if (this.scatterPlot.zColumn !== null && this.scatterPlot.zColumn.categories && dim === this.scatterPlot.zColumn.dim) {
          const catIndex = row[this.scatterPlot.zColumn.dim];
          const category = this.scatterPlot.zColumn.categories[catIndex];
          const coloringHtml = `<span class='swatch' style='background:${this.scatterPlot.pointColor(row)}'>&nbsp;</span>`;
          const axisName = dim === this.scatterPlot.xColumn.dim || dim === this.scatterPlot.yColumn.dim ? "axisName" : "zAxisName";
          tooltip.append("div").html(`<span class="${axisName}">${this.scatterPlot.zColumn.labelText()}</span> ${coloringHtml}&nbsp;: ${_PointsPlot.brIze(category)}`);
        } else if (dim === this.scatterPlot.xColumn.dim) {
          tooltip.append("div").html(`<span class="axisName">${column.labelText()}</span>: <span class="xValue">${column.formatedRowValue(row)}</span>`);
        } else if (dim === this.scatterPlot.yColumn.dim) {
          tooltip.append("div").html(`<span class="axisName">${column.labelText()}</span>: <span class="yValue">${column.formatedRowValue(row)}</span>`);
        } else {
          tooltip.append("div").html(`${column.labelText()}: ${column.formatedRowValue(row)}`);
        }
      });
    }
    static brIze(category) {
      const splittedCategory = category.toString().split(", ");
      if (splittedCategory.length > 1) {
        return "<br>" + splittedCategory.join(",<br>") + "<br>";
      }
      return splittedCategory[0];
    }
    tooltipLocation() {
      const mspDivNode = select_default2(this.scatterPlot.bindto + " .mspDiv").node();
      const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
      const xParent = parentBounds === null ? 0 : parentBounds.x;
      const plotGroup = select_default2(this.scatterPlot.bindto + " .mspGroup").node();
      const elementBounds = plotGroup === null ? null : plotGroup.getBoundingClientRect();
      const xRect = elementBounds === null ? 0 : elementBounds.x;
      const wRect = elementBounds === null ? 0 : elementBounds.width;
      return [xRect - xParent + wRect + 5, this.scatterPlot.yPlot];
    }
    mouseout() {
      if (this.scatterPlot.mouseMode !== SpConst.tooltipMouse.key) {
        return;
      }
      select_default2(this.scatterPlot.bindto + " .mspTooltip").style("display", "none");
    }
    addAlpha(color2) {
      const d3Color = color(color2);
      if (d3Color) {
        d3Color.opacity = this.scatterPlot.style.plotProperties.point.alpha;
        return d3Color.toString();
      }
      return color2;
    }
    drawCanvas(picking) {
      const canvasSelector = this.scatterPlot.canvasSelector(picking);
      const canvasNode = select_default2(canvasSelector).node();
      if (!canvasNode) {
        console.error("canvasNode is null for:", canvasSelector);
        return;
      }
      const context2d = canvasNode.getContext("2d", { willReadFrequently: true });
      if (!context2d) {
        console.error("context2d is null");
        return;
      }
      const thisPlot = this;
      const xScaleRange = this.scatterPlot.xScale.range();
      const yScaleRange = this.scatterPlot.yScale.range();
      const xPlot = -xScaleRange[0];
      const yPlot = -yScaleRange[1];
      this.scatterPlot.spData.sampleData.forEach(function(row, i) {
        if (!thisPlot.scatterPlot.spData.cutRows[i]) {
          if (picking) {
            thisPlot.drawPickingPoint(row, i, xPlot, yPlot, context2d);
          } else {
            thisPlot.drawPoint(row, i, xPlot, yPlot, context2d, true);
          }
        }
      });
      this.scatterPlot.spData.sampleData.forEach(function(row, i) {
        if (thisPlot.scatterPlot.spData.cutRows[i]) {
          if (picking) {
            thisPlot.drawPickingPoint(row, i, xPlot, yPlot, context2d);
          } else {
            thisPlot.drawPoint(row, i, xPlot, yPlot, context2d, false);
          }
        }
      });
    }
    drawPoint(row, rowIndex, xPlot, yPlot, context2d, useWatermark) {
      const cx = this.scatterPlot.cx(row, rowIndex) + xPlot;
      const cy = this.scatterPlot.cy(row, rowIndex) + yPlot;
      const r = this.scatterPlot.style.plotProperties.point.radius;
      context2d.beginPath();
      context2d.arc(cx, cy, r, 0, 2 * Math.PI, false);
      context2d.fillStyle = useWatermark ? this.scatterPlot.style.plotProperties.watermarkColor : this.addAlpha(this.scatterPlot.pointColor(row));
      context2d.fill();
    }
    drawPickingPoint(row, rowIndex, xPlot, yPlot, context2d) {
      const size = 2 * this.scatterPlot.style.plotProperties.point.radius;
      const x2 = Math.round(this.scatterPlot.cx(row, rowIndex) + xPlot - this.scatterPlot.style.plotProperties.point.radius);
      const y2 = Math.round(this.scatterPlot.cy(row, rowIndex) + yPlot - this.scatterPlot.style.plotProperties.point.radius);
      context2d.beginPath();
      context2d.rect(x2, y2, size, size);
      context2d.fillStyle = _PointsPlot.pickingColor(rowIndex);
      context2d.fill();
    }
    static pickingColor(dataIndex) {
      const hex2 = "00000" + dataIndex.toString(16);
      const pickingColor = "#" + hex2.substring(hex2.length - 6);
      return pickingColor;
    }
  };

  // src/Typescript/scatterPlot.ts
  var ScatterPlot = class _ScatterPlot {
    constructor(spData, config) {
      this.xPlot = 0;
      this.yPlot = 0;
      this.width = 0;
      this.height = 0;
      this.xScale = linear2();
      this.axisVisibility = { xTitle: true, xValues: true, yTitle: true, yValues: true };
      this.scatterXAxis = axisBottom(this.xScale).tickFormat(_ScatterPlot.prototype.formatXValue.bind(this));
      this.yScale = linear2();
      this.scatterYAxis = axisLeft(this.yScale).tickFormat(_ScatterPlot.prototype.formatYValue.bind(this));
      this.zScale = linear2();
      this.continuousCslAxis = axisRight(this.zScale).tickFormat(_ScatterPlot.prototype.formatZValue.bind(this));
      // eslint-disable-next-line no-use-before-define
      this.zoomBrush = null;
      this.dblClickTimeout = null;
      this.regressionPlot = null;
      this.xDistribPlot = null;
      this.yDistribPlot = null;
      this.horViolinPlots = [];
      this.verViolinPlots = [];
      this.pickingReady = false;
      this.spData = spData;
      this.style = config.style;
      this.bindto = config.bindto;
      this.index = config.index;
      this.xColumn = spData.columns[spData.dimensions[0]];
      this.yColumn = spData.columns[spData.dimensions[0]];
      this.zColumn = null;
      this.row = config.row;
      this.col = config.col;
      this.regressionType = config.regressionType;
      this.mouseMode = config.mouseMode;
      this.continuousCsId = config.continuousCsId;
      this.categoricalCsId = config.categoricalCsId;
      this.distribVisibility = config.distribVisibility;
      this.distribType = config.distribType;
      this.axisVisibility = config.axisVisibility;
      this.multiBrush = null;
      this.contColorScale = sequential(SpConst.CONTINUOUS_CS[this.continuousCsId]);
      this.catColorScale = SpConst.CATEGORIAL_CS[this.categoricalCsId];
    }
    static {
      this.padding = { r: 10, t: 10 };
    }
    static {
      this.DISTRIB_RATIO = 0.15;
    }
    static {
      this.margin = { l: 60, r: 10, b: 50, t: 5 };
    }
    static {
      this.cslRight = 30;
    }
    static {
      this.cslLeft = 10;
    }
    static {
      this.cslWidth = 20;
    }
    static {
      this.cslTotalWidth = _ScatterPlot.cslRight + _ScatterPlot.cslLeft + _ScatterPlot.cslWidth;
    }
    static {
      this.INIT = 1;
    }
    static {
      this.SHAPE = 1 << 1;
    }
    static {
      this.PALETTE = 1 << 2;
    }
    static {
      this.Z_AXIS = 1 << 3;
    }
    static {
      this.RANGE = 1 << 4;
    }
    static {
      this.DOMAIN = 1 << 5;
    }
    removePlot() {
      this.plotSelection().remove();
      select_default2(this.canvasSelector(true)).remove();
      select_default2(this.canvasSelector(false)).remove();
      this.spData.on(SpData.HL_POINT_EVENT + "." + this.index, null);
    }
    setXColumn(column) {
      this.xColumn = column;
      this.xDistribPlot = null;
      this.horViolinPlots = [];
      this.verViolinPlots = [];
    }
    setYColumn(column) {
      this.yColumn = column;
      this.yDistribPlot = null;
      this.horViolinPlots = [];
      this.verViolinPlots = [];
    }
    setZColumn(column) {
      this.zColumn = column;
    }
    formatXValue(value) {
      return this.xColumn.formatedValue(value);
    }
    formatYValue(value) {
      return this.yColumn.formatedValue(value);
    }
    formatZValue(value) {
      return this.zColumn === null ? "No Z axis" : this.zColumn.formatedValue(value);
    }
    // eslint-disable-next-line max-lines-per-function
    draw(updateType) {
      const plotSelection = this.plotSelection();
      this.updateScales();
      if (updateType & _ScatterPlot.INIT) {
        const x2 = (this.width * this.distribRatio() + this.width - _ScatterPlot.padding.r) / 2;
        const y2 = (this.height * (1 - this.distribRatio()) + _ScatterPlot.padding.t) / 2;
        plotSelection.append("text").attr("class", "scatterNumber").attr("x", x2).attr("y", y2).attr("text-anchor", "middle").attr("dominant-baseline", "middle").text(this.index + 1);
      }
      this.drawXAxis(updateType, plotSelection);
      this.drawYAxis(updateType, plotSelection);
      this.drawJitterZones(updateType, plotSelection);
      if (updateType & _ScatterPlot.INIT) {
        const xScaleRange = this.xScale.range();
        const yScaleRange = this.yScale.range();
        const spRect = plotSelection.select(".spArea").append("rect").attr("class", "spRect").attr("x", xScaleRange[0]).attr("y", yScaleRange[1]).attr("width", xScaleRange[1] - xScaleRange[0]).attr("height", yScaleRange[0] - yScaleRange[1]).attr("fill", "none").attr("pointer-events", "fill").on("mousemove", (event) => {
          this.spData.dispatch.call(SpData.HL_GRAPH_EVENT, void 0, this);
          this.canvasMousemove(pointer_default(event));
        }).on("mouseout", (event) => {
          this.canvasMouseout();
          const coord = pointer_default(event);
          if (coord[0] < xScaleRange[0] || coord[0] > xScaleRange[1] || coord[1] < yScaleRange[1] || coord[0] > yScaleRange[0]) {
            this.spData.dispatch.call(SpData.HL_GRAPH_EVENT, void 0, null);
          }
        }).on("click", (event) => {
          this.canvasClick(pointer_default(event));
        });
        const mspDivNode = select_default2(this.bindto + " .mspDiv").node();
        const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
        const xParent = parentBounds === null ? 0 : parentBounds.x;
        const yParent = parentBounds === null ? 0 : parentBounds.y;
        const spRectNode = spRect.node();
        const spRectBounds = spRectNode === null ? null : spRectNode.getBoundingClientRect();
        const xSpRect = spRectBounds === null ? 0 : spRectBounds.x;
        const ySpRect = spRectBounds === null ? 0 : spRectBounds.y;
        this.xPlot = xSpRect - xParent;
        this.yPlot = ySpRect - yParent;
        const thisPlot = this;
        this.spData.on(SpData.HL_POINT_EVENT + "." + this.index, function(hlEvent) {
          if (thisPlot.plotSelection().size() !== 0) {
            thisPlot.hlPoint(thisPlot.plotSelection(), hlEvent);
          }
        });
        plotSelection.select(".spArea").append("foreignObject").style("pointer-events", "none").attr("class", "canvasGroup" + this.index + " drawing").attr("x", xScaleRange[0]).attr("y", yScaleRange[1]).attr("width", xScaleRange[1] - xScaleRange[0]).attr("height", yScaleRange[0] - yScaleRange[1]).append("xhtml:div");
        plotSelection.select(".spArea").append("foreignObject").style("pointer-events", "none").attr("class", "canvasGroup" + this.index + " picking").attr("x", xScaleRange[0]).attr("y", yScaleRange[1]).attr("width", xScaleRange[1] - xScaleRange[0]).attr("height", yScaleRange[0] - yScaleRange[1]).append("xhtml:div");
      }
      this.drawRegressionPlots(updateType, plotSelection);
      this.drawDistribPlots(updateType, plotSelection);
      this.drawVerViolinPlots(updateType, plotSelection);
      this.drawHorViolinPlots(updateType, plotSelection);
      this.drawCsl(updateType, plotSelection);
      this.drawBrush(updateType, plotSelection);
      this.drawCanvas(false);
    }
    hlPoint(plotSelection, hlEvent) {
      const i = hlEvent.pointIndex;
      const cx = i === null ? 0 : this.cx(this.spData.sampleData[i], i);
      const cy = i === null ? 0 : this.cy(this.spData.sampleData[i], i);
      const color2 = i === null ? "black" : this.pointColor(this.spData.sampleData[i]);
      const spArea = plotSelection.select(".spArea");
      spArea.selectAll(".hlPoint").data(["hlPoint"]).join(
        (enter) => enter.append("circle").attr("class", "hlPoint").style("pointer-events", "none"),
        (update) => update,
        (exit) => exit.remove()
      ).attr("fill", color2).attr("cx", cx).attr("cy", cy).attr("r", 2 * this.style.plotProperties.point.radius).attr("display", i === null ? "none" : "block");
    }
    cx(row, i) {
      const cx = this.xColumn.categories === null ? this.xScale(row[this.xColumn.dim]) : this.xScale(row[this.xColumn.dim] + this.spData.jitterXValues[i]);
      return typeof cx === "undefined" ? NaN : cx;
    }
    cy(row, i) {
      const cy = this.yColumn.categories === null ? this.yScale(row[this.yColumn.dim]) : this.yScale(row[this.yColumn.dim] + this.spData.jitterYValues[i]);
      return typeof cy === "undefined" ? NaN : cy;
    }
    pointColor(row) {
      if (this.zColumn !== null) {
        const pointColor = this.zColumn.categories === null ? this.contColorScale(row[this.zColumn.dim]) : this.catColorScale(row[this.zColumn.dim]);
        if (typeof pointColor !== "undefined") {
          return pointColor;
        }
      }
      return this.style.plotProperties.noCatColor;
    }
    hlGraph(highlight) {
      const plotSelection = this.plotSelection();
      plotSelection.select(".spRect").classed("hlGraph", highlight);
    }
    plotSelection(plotSelection) {
      if (plotSelection) {
        return plotSelection;
      }
      const thisPlot = this;
      const mspGroup = select_default2(this.bindto + " .mspGroup");
      return mspGroup.selectAll(".scatterPlot").filter(function(plot) {
        return plot.row === thisPlot.row && plot.col === thisPlot.col;
      });
    }
    drawRegressionPlots(updateType, plotSelection) {
      const spArea = plotSelection.select(".spArea");
      if (updateType & _ScatterPlot.INIT || !this.regressionPlot) {
        this.regressionPlot = new RegressionPlot(this);
        this.regressionPlot.generate(spArea);
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.SHAPE | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.Z_AXIS)) {
        this.regressionPlot.computePlot(this.zColumn);
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.PALETTE)) {
        this.regressionPlot.colorScale(this.catColorScale);
      }
      this.regressionPlot.update(updateType, spArea);
    }
    // eslint-disable-next-line max-lines-per-function
    drawDistribPlots(updateType, plotSelection) {
      if (!this.distribVisibility) {
        return;
      }
      const distribGroup = plotSelection.select(".distribGroup");
      if (updateType & _ScatterPlot.INIT || !this.xDistribPlot) {
        this.xDistribPlot = new DistributionPlot(
          this.spData,
          this.xColumn,
          {
            bindto: this.bindto,
            orientation: DistributionPlot.HOR,
            mouseMode: this.mouseMode,
            categoricalCsId: this.categoricalCsId,
            distribType: this.distribType,
            style: this.style
          },
          this.index,
          this.xPlot,
          this.yPlot
        );
        this.xDistribPlot.generate(distribGroup, `${this.bindto}-x-clip`);
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.SHAPE | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.Z_AXIS)) {
        const xDistribPlotRange = [this.height, this.height * (1 - this.distribRatio() * 0.8)];
        this.xDistribPlot.valuesScaleRange(this.xScale.range()).computePlot(this.zColumn).distribScaleRange(xDistribPlotRange);
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.PALETTE)) {
        this.xDistribPlot.colorScale(this.catColorScale);
      }
      this.xDistribPlot.update(updateType, distribGroup);
      if (updateType & _ScatterPlot.INIT || !this.yDistribPlot) {
        this.yDistribPlot = new DistributionPlot(
          this.spData,
          this.yColumn,
          {
            bindto: this.bindto,
            orientation: DistributionPlot.VER,
            mouseMode: this.mouseMode,
            categoricalCsId: this.categoricalCsId,
            distribType: this.distribType,
            style: this.style
          },
          this.index,
          this.xPlot,
          this.yPlot
        );
        this.yDistribPlot.generate(distribGroup, `${this.bindto}-y-clip`);
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.SHAPE | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.Z_AXIS)) {
        const yDistribPlotRange = [0, this.width * this.distribRatio() * 0.8];
        this.yDistribPlot.valuesScaleRange(this.yScale.range()).computePlot(this.zColumn).distribScaleRange(yDistribPlotRange);
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.PALETTE)) {
        this.yDistribPlot.colorScale(this.catColorScale);
      }
      this.yDistribPlot.update(updateType, distribGroup);
    }
    distribRatio() {
      return this.distribVisibility ? _ScatterPlot.DISTRIB_RATIO : 0;
    }
    drawVerViolinPlots(updateType, plotSelection) {
      const thisPlot = this;
      const spArea = plotSelection.select(".spArea");
      const xCategories = this.xColumn.categories ? this.xColumn.categories : [];
      if (updateType & _ScatterPlot.INIT || this.verViolinPlots.length === 0) {
        this.verViolinPlots = xCategories.map((_cat) => new DistributionPlot(
          this.spData,
          this.yColumn,
          {
            bindto: this.bindto,
            orientation: DistributionPlot.VER,
            mouseMode: this.mouseMode,
            categoricalCsId: this.categoricalCsId,
            distribType: this.distribType,
            style: this.style
          },
          this.index,
          this.xPlot,
          this.yPlot
        ));
      }
      spArea.selectAll(".ver.violinGroup").data(this.verViolinPlots).join(
        (enter) => enter.append("g").attr("class", "ver violinGroup").each(function(violinPlot) {
          violinPlot.generate(select_default2(this));
        }),
        (update) => update,
        (exit) => exit.remove()
      ).each(function(violinPlot, i) {
        if (updateType & (_ScatterPlot.INIT | _ScatterPlot.SHAPE | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.Z_AXIS)) {
          const violinRange = _ScatterPlot.verViolinRange(thisPlot.xScale, i, xCategories.length);
          violinPlot.valuesScaleRange(thisPlot.yScale.range()).computePlot(thisPlot.zColumn, { column: thisPlot.xColumn, catIndex: i }).distribScaleRange(violinRange);
        }
        if (updateType & (_ScatterPlot.INIT | _ScatterPlot.PALETTE | _ScatterPlot.Z_AXIS)) {
          violinPlot.colorScale(thisPlot.catColorScale);
        }
        violinPlot.update(updateType, select_default2(this));
      });
    }
    drawHorViolinPlots(updateType, plotSelection) {
      const thisPlot = this;
      const spArea = plotSelection.select(".spArea");
      const yCategories = this.yColumn.categories ? this.yColumn.categories : [];
      if (updateType & _ScatterPlot.INIT || this.horViolinPlots.length === 0) {
        this.horViolinPlots = yCategories.map((_cat) => new DistributionPlot(
          this.spData,
          this.xColumn,
          {
            bindto: this.bindto,
            orientation: DistributionPlot.HOR,
            mouseMode: this.mouseMode,
            categoricalCsId: this.categoricalCsId,
            distribType: this.distribType,
            style: this.style
          },
          this.index,
          this.xPlot,
          this.yPlot
        ));
      }
      spArea.selectAll(".hor.violinGroup").data(this.horViolinPlots).join(
        (enter) => enter.append("g").attr("class", "hor violinGroup").each(function(violinPlot) {
          violinPlot.generate(select_default2(this));
        }),
        (update) => update,
        (exit) => exit.remove()
      ).each(function(violinPlot, i) {
        if (updateType & (_ScatterPlot.INIT | _ScatterPlot.SHAPE | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.Z_AXIS)) {
          const violinRange = _ScatterPlot.horViolinRange(thisPlot.yScale, i, yCategories.length);
          violinPlot.valuesScaleRange(thisPlot.xScale.range()).computePlot(thisPlot.zColumn, { column: thisPlot.yColumn, catIndex: i }).distribScaleRange(violinRange);
        }
        if (updateType & (_ScatterPlot.INIT | _ScatterPlot.PALETTE | _ScatterPlot.Z_AXIS)) {
          violinPlot.colorScale(thisPlot.catColorScale);
        }
        violinPlot.update(updateType, select_default2(this));
      });
    }
    updateScales() {
      this.updateXScale();
      this.updateYScale();
      this.updateZScale();
    }
    updateXScale() {
      this.xScale.range([this.width * this.distribRatio(), this.width - _ScatterPlot.padding.r]).domain(this.xColumn.domain());
      if (this.xColumn.categories === null) {
        this.xScale.nice();
      }
      const xTickSize = -this.height + _ScatterPlot.padding.t;
      this.scatterXAxis.scale(this.xScale).ticks(this.xColumn.axisTicks()).tickSize(xTickSize);
    }
    updateYScale() {
      this.yScale.range([this.height * (1 - this.distribRatio()), _ScatterPlot.padding.t]).domain(this.yColumn.domain());
      if (this.yColumn.categories === null) {
        this.yScale.nice();
      }
      const yTickSize = -this.width + _ScatterPlot.padding.r;
      this.scatterYAxis.scale(this.yScale).ticks(this.yColumn.axisTicks()).tickSize(yTickSize);
    }
    updateZScale() {
      const zColumn = this.zColumn;
      if (zColumn === null) {
        return;
      }
      this.zScale.range([this.height * (1 - this.distribRatio()) - _ScatterPlot.padding.t, 0]).domain(zColumn.domain());
      if (zColumn.categories === null) {
        this.zScale.nice();
        const [zMin, zMax] = zColumn.domain();
        this.contColorScale = sequential(SpConst.CONTINUOUS_CS[this.continuousCsId]).domain([zMin, zMax]);
        this.continuousCslAxis.scale(this.zScale).ticks(zColumn.axisTicks());
      } else {
        const zMax = zColumn.domain()[1];
        this.catColorScale = SpConst.CATEGORIAL_CS[this.categoricalCsId].domain(range(zMax));
      }
    }
    drawCsl(updateType, plotSelection) {
      if (updateType & _ScatterPlot.INIT) {
        plotSelection.append("g").attr("class", "cslGroup");
      } else {
        plotSelection.select(".cslGroup > g").remove();
      }
      plotSelection.select(".cslGroup").style("visibility", this.zColumn === null ? "hidden" : "visible");
      const zColumn = this.zColumn;
      if (zColumn === null) {
        return;
      }
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.PALETTE | _ScatterPlot.Z_AXIS | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN)) {
        if (zColumn.categories === null) {
          this.drawContinuousCsl(plotSelection);
        } else {
          this.drawCategoricalCsl(plotSelection);
        }
        plotSelection.select(".cslGroup").attr("transform", "translate(" + (this.width + _ScatterPlot.cslLeft) + ", " + _ScatterPlot.padding.t + ")");
        _ScatterPlot.tspanColumnTitleBT(plotSelection.select(".cslTitle"), zColumn);
      }
    }
    static tspanColumnTitleBT(textSelection, column) {
      const labels = column.label.split("<br>");
      textSelection.text(labels[labels.length - 1]);
      for (let i = 1; i < labels.length; i++) {
        textSelection.append("tspan").attr("x", 0).attr("dy", "-1em").text(labels[labels.length - 1 - i]);
      }
    }
    static tspanColumnTitleTB(textSelection, column) {
      const labels = column.label.split("<br>");
      textSelection.text(labels[0]);
      for (let i = 1; i < labels.length; i++) {
        textSelection.append("tspan").attr("x", 0).attr("dy", "1em").text(labels[i]);
      }
    }
    drawContinuousCsl(plotSelection) {
      const thisPlot = this;
      const cslGroup = plotSelection.select(".cslGroup");
      const continuousCslGroup = cslGroup.append("g").attr("class", "continuous");
      const colorScaleBars = continuousCslGroup.append("g").attr("class", "colorScaleBars");
      const csHeight = thisPlot.zScale.range()[0] - thisPlot.zScale.range()[1];
      colorScaleBars.selectAll(".colorScaleBar").data(range(csHeight)).enter().append("rect").attr("class", "colorScaleBar").attr("y", function(_d, i) {
        return i;
      }).attr("width", _ScatterPlot.cslWidth).attr("height", 1).style("fill", function(pixel) {
        const fill = thisPlot.contColorScale(
          thisPlot.zScale.invert(pixel)
        );
        return typeof fill === "undefined" ? "black" : fill;
      });
      continuousCslGroup.append("rect").attr("class", "continuousCslRect").attr("width", _ScatterPlot.cslWidth).attr("height", csHeight);
      continuousCslGroup.append("g").attr("class", "continuousCslAxis").attr("transform", "translate(" + _ScatterPlot.cslWidth + ", 0)").call(this.continuousCslAxis);
      continuousCslGroup.append("text").attr("class", "cslTitle").attr("x", 0).attr("y", -7);
    }
    drawCategoricalCsl(plotSelection) {
      const zColumn = this.zColumn;
      if (zColumn === null || !zColumn.categories) {
        console.error("'drawCategoricalCsl' called, but Z column is not categorial");
        return;
      }
      const thisPlot = this;
      const cslGroup = plotSelection.select(".cslGroup");
      const catGroupHeight = 15;
      const yCatGroup = 0.5 * (this.height - zColumn.categories.length * catGroupHeight);
      const categorialCslGroup = cslGroup.append("g").attr("transform", `translate(0,${yCatGroup < 0 ? 0 : yCatGroup})`);
      categorialCslGroup.append("text").attr("class", "cslTitle").attr("x", 0).attr("y", -7);
      const categoryGroup = categorialCslGroup.selectAll(".categoryGroup").data(zColumn.categories).enter().append("g").attr("class", "categoryGroup").attr("transform", (_cat, i) => `translate(0,${i * catGroupHeight})`);
      categoryGroup.append("rect").attr("width", 10).attr("height", 10).style("fill", function(_cat, i) {
        return thisPlot.catColorScale(i);
      });
      categoryGroup.append("text").attr("x", 15).attr("y", 5).attr("dy", "0.35em").text(function(cat) {
        return cat;
      });
    }
    drawXAxis(updateType, plotSelection) {
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.SHAPE)) {
        const axesGroup = plotSelection.select(".axesGroup");
        if (updateType & _ScatterPlot.INIT) {
          axesGroup.append("g").attr("class", "x axis").attr("transform", "translate(0," + this.height + ")");
          if (this.axisVisibility.xTitle) {
            const x2 = (this.width * this.distribRatio() + this.width - _ScatterPlot.padding.r) / 2;
            const y2 = this.height + _ScatterPlot.margin.b - 5;
            axesGroup.append("text").attr("class", "x scatterlabel").attr("x", x2).attr("y", y2).attr("text-anchor", "middle").attr("dominant-baseline", "middle");
          }
        }
        axesGroup.select(".x.axis").call(this.scatterXAxis).selectAll(".tick text").attr("transform", "rotate(45)").style("text-anchor", "start").attr("display", this.axisVisibility.xValues ? "block" : "none");
        if (this.axisVisibility.xTitle) {
          axesGroup.select(".x.scatterlabel").text(this.xColumn.labelText()).classed("input", this.xColumn.isInput()).classed("output", this.xColumn.isOutput());
        }
      }
    }
    drawYAxis(updateType, plotSelection) {
      if (updateType & (_ScatterPlot.INIT | _ScatterPlot.RANGE | _ScatterPlot.DOMAIN | _ScatterPlot.SHAPE)) {
        const axesGroup = plotSelection.select(".axesGroup");
        if (updateType & _ScatterPlot.INIT) {
          axesGroup.append("g").attr("class", "y axis");
          if (this.axisVisibility.yTitle) {
            const x2 = -_ScatterPlot.margin.l * 0.7;
            const y2 = (this.height * (1 - this.distribRatio()) + _ScatterPlot.padding.t) / 2;
            axesGroup.append("text").attr("class", "y scatterlabel").attr("transform", "translate(" + x2 + "," + y2 + ")rotate(270)").attr("dominant-baseline", "baseline").attr("text-anchor", "middle");
          }
        }
        axesGroup.select(".y.axis").call(this.scatterYAxis).selectAll(".tick text").attr("display", this.axisVisibility.yValues ? "block" : "none");
        if (this.axisVisibility.yTitle) {
          axesGroup.select(".y.scatterlabel").text(this.yColumn.labelText()).classed("input", this.yColumn.isInput()).classed("output", this.yColumn.isOutput());
        }
      }
    }
    // eslint-disable-next-line max-lines-per-function
    drawJitterZones(_updateType, plotSelection) {
      const thisPlot = this;
      const spArea = plotSelection.select(".spArea");
      const xZoneRange = this.xColumn.categories ? this.xColumn.categories.map((_cat, i) => [thisPlot.xScale(i - 0.5 / SpConst.CAT_RATIO), thisPlot.xScale(i + 0.5 / SpConst.CAT_RATIO)].map(_ScatterPlot.undef2Nan)) : [thisPlot.xScale.range()];
      const yZoneRange = this.yColumn.categories ? this.yColumn.categories.map((_cat, i) => [thisPlot.yScale(i - 0.5 / SpConst.CAT_RATIO), thisPlot.yScale(i + 0.5 / SpConst.CAT_RATIO)].map(_ScatterPlot.undef2Nan)) : [thisPlot.yScale.range()];
      const jitterZonesIndexes = [];
      for (let i = 0; i < xZoneRange.length; i++) {
        for (let j = 0; j < yZoneRange.length; j++) {
          jitterZonesIndexes.push([i, j]);
        }
      }
      spArea.selectAll(".jitterZone").data(range(jitterZonesIndexes.length)).join(
        (enter) => enter.append("rect").attr("class", "jitterZone"),
        (update) => update,
        (exit) => exit.remove()
      ).attr("x", function(zoneIndex) {
        const index = jitterZonesIndexes[zoneIndex][0];
        return Math.min(xZoneRange[index][0], xZoneRange[index][1]);
      }).attr("y", function(zoneIndex) {
        const index = jitterZonesIndexes[zoneIndex][1];
        return Math.min(yZoneRange[index][0], yZoneRange[index][1]);
      }).attr("width", function(zoneIndex) {
        const index = jitterZonesIndexes[zoneIndex][0];
        return Math.abs(xZoneRange[index][1] - xZoneRange[index][0]);
      }).attr("height", function(zoneIndex) {
        const index = jitterZonesIndexes[zoneIndex][1];
        return Math.abs(yZoneRange[index][1] - yZoneRange[index][0]);
      });
    }
    setContCutoff(brushSelections) {
      const xyCutoffs = brushSelections.map((brushSelection2) => [
        [brushSelection2[0][0], brushSelection2[1][0]].map(this.xScale.invert).sort((a2, b) => a2 - b),
        [brushSelection2[0][1], brushSelection2[1][1]].map(this.yScale.invert).sort((a2, b) => a2 - b)
      ]);
      if (xyCutoffs === null || xyCutoffs.length === 0) {
        this.spData.setXYCutoffs(this.xColumn.dim, this.yColumn.dim, null);
      } else {
        this.spData.setXYCutoffs(this.xColumn.dim, this.yColumn.dim, xyCutoffs);
      }
      this.spData.dispatchRowFilterEvent();
    }
    fixBrush() {
      const plotSelection = this.plotSelection();
      const multiBrushGroup = select_default2(this.bindto + " ." + MultiBrush.multiBrushClass(this.index));
      if (this.mouseMode === SpConst.filterMouse.key) {
        multiBrushGroup.selectAll(".selection").style("display", null);
        multiBrushGroup.selectAll(".handle").style("display", null);
        multiBrushGroup.selectAll(".overlay").style("pointer-events", "all");
      } else {
        multiBrushGroup.selectAll(".selection").style("display", "none");
        multiBrushGroup.selectAll(".handle").style("display", "none");
        multiBrushGroup.selectAll(".overlay").style("pointer-events", "auto");
      }
      const zoomBrushGroup = select_default2(this.bindto + " ." + this.zoomBrushClass());
      if (this.mouseMode === SpConst.zoomMouse.key) {
        zoomBrushGroup.selectAll(".selection").style("display", null);
        zoomBrushGroup.selectAll(".handle").style("display", null);
        zoomBrushGroup.selectAll(".overlay").style("pointer-events", "all");
      } else {
        zoomBrushGroup.selectAll(".selection").style("display", "none");
        zoomBrushGroup.selectAll(".handle").style("display", "none");
        zoomBrushGroup.selectAll(".overlay").style("pointer-events", "auto");
      }
      if (this.mouseMode === SpConst.filterMouse.key) {
        this.drawBrush(_ScatterPlot.RANGE, plotSelection);
      }
    }
    drawBrush(updateType, plotSelection) {
      const thisPlot = this;
      if (updateType & _ScatterPlot.INIT) {
        plotSelection.select(".spArea").append("g").attr("class", function() {
          return MultiBrush.multiBrushClass(thisPlot.index);
        });
        plotSelection.select(".spArea").append("g").attr("class", function() {
          return thisPlot.zoomBrushClass();
        });
        this.initZoomBrush();
      }
      if (thisPlot.mouseMode === SpConst.filterMouse.key) {
        if (this.multiBrush === null || select_default2(this.bindto).select("." + MultiBrush.multiBrushClass(this.index)).selectAll(".brush").size() === 0) {
          this.multiBrush = new MultiBrush(this);
        }
        this.multiBrush.initFrom(this.spData.getRowFilter(this.xColumn.dim, this.yColumn.dim).xyCutoffs);
      }
    }
    initZoomBrush() {
      const thisPlot = this;
      this.zoomBrush = brush_default().on("end", (event) => {
        const brushZone = event.selection;
        thisPlot.applyZoom(brushZone);
      });
      const xExtent = [
        thisPlot.xScale.range()[0],
        thisPlot.yScale.range()[1]
      ];
      const yExtent = [
        thisPlot.xScale.range()[1],
        thisPlot.yScale.range()[0]
      ];
      this.zoomBrush.extent([xExtent, yExtent]);
      select_default2(this.bindto + " ." + this.zoomBrushClass()).call(this.zoomBrush);
    }
    // eslint-disable-next-line max-lines-per-function
    applyZoom(brushZone) {
      if (!this.zoomBrush) {
        return;
      }
      const thisPlot = this;
      if (!brushZone && !this.dblClickTimeout) {
        this.dblClickTimeout = setTimeout(function() {
          thisPlot.dblClickTimeout = null;
        }, SpConst.dblClickDelay);
        return;
      }
      const plotSelection = this.plotSelection();
      if (brushZone) {
        this.xScale.domain([brushZone[0][0], brushZone[1][0]].map(this.xScale.invert));
        this.yScale.domain([brushZone[1][1], brushZone[0][1]].map(this.yScale.invert));
        select_default2(this.bindto + " ." + this.zoomBrushClass()).call(this.zoomBrush.clear);
      } else {
        this.xScale.domain(this.xColumn.domain());
        this.yScale.domain(this.yColumn.domain());
      }
      if (this.xColumn.categories === null) {
        this.xScale.nice();
      }
      if (this.yColumn.categories === null) {
        this.yScale.nice();
      }
      this.drawXAxis(_ScatterPlot.DOMAIN, plotSelection);
      this.drawYAxis(_ScatterPlot.DOMAIN, plotSelection);
      this.drawJitterZones(_ScatterPlot.DOMAIN, plotSelection);
      this.drawBrush(_ScatterPlot.DOMAIN, plotSelection);
      if (this.regressionPlot) {
        this.regressionPlot.update(_ScatterPlot.DOMAIN, plotSelection);
      }
      this.drawCanvas(false);
      if (this.xDistribPlot && this.distribVisibility) {
        this.xDistribPlot.valuesScale.domain(this.xScale.domain());
        this.xDistribPlot.update(_ScatterPlot.DOMAIN, plotSelection.select(".distribGroup"));
      }
      if (this.yDistribPlot && this.distribVisibility) {
        this.yDistribPlot.valuesScale.domain(this.yScale.domain());
        this.yDistribPlot.update(_ScatterPlot.DOMAIN, plotSelection.select(".distribGroup"));
      }
      if (this.xColumn.categories) {
        const xCategories = this.xColumn.categories;
        const fullXScale = linear2().range(this.xScale.range()).domain(this.xColumn.domain());
        plotSelection.selectAll(".ver.violinGroup").each(function(violinPlot, i) {
          const violinRange = _ScatterPlot.verViolinRange(fullXScale, i, xCategories.length);
          if (brushZone) {
            const range2 = violinRange.map(fullXScale.invert).map(thisPlot.xScale).map(_ScatterPlot.undef2Nan);
            violinPlot.distribScaleRange(range2);
          } else {
            violinPlot.distribScaleRange(violinRange);
          }
          violinPlot.valuesScale.domain(thisPlot.yScale.domain());
          violinPlot.update(_ScatterPlot.DOMAIN, select_default2(this));
        });
      }
      if (this.yColumn.categories) {
        const yCategories = this.yColumn.categories;
        const fullYScale = linear2().range(this.yScale.range()).domain(this.yColumn.domain());
        plotSelection.selectAll(".hor.violinGroup").each(function(violinPlot, i) {
          const violinRange = _ScatterPlot.horViolinRange(fullYScale, i, yCategories.length);
          if (brushZone) {
            const range2 = violinRange.map(fullYScale.invert).map(thisPlot.yScale).map(_ScatterPlot.undef2Nan);
            violinPlot.distribScaleRange(range2);
          } else {
            violinPlot.distribScaleRange(violinRange);
          }
          violinPlot.valuesScale.domain(thisPlot.xScale.domain());
          violinPlot.update(_ScatterPlot.DOMAIN, select_default2(this));
        });
      }
    }
    zoomBrushClass() {
      return "zoombrush_plot" + this.index;
    }
    distribRepChange(newType) {
      this.distribType = newType;
      if (this.xDistribPlot) {
        this.xDistribPlot.distribType = newType;
      }
      if (this.yDistribPlot) {
        this.yDistribPlot.distribType = newType;
      }
      this.verViolinPlots.forEach(function(violinPlot) {
        violinPlot.distribType = newType;
      });
      this.horViolinPlots.forEach(function(violinPlot) {
        violinPlot.distribType = newType;
      });
      const plotSelection = this.plotSelection();
      this.drawDistribPlots(_ScatterPlot.SHAPE, plotSelection);
      this.drawVerViolinPlots(_ScatterPlot.SHAPE, plotSelection);
      this.drawHorViolinPlots(_ScatterPlot.SHAPE, plotSelection);
    }
    regressionRepChange(newType) {
      this.regressionType = newType;
      const plotSelection = this.plotSelection();
      this.drawRegressionPlots(_ScatterPlot.SHAPE, plotSelection);
    }
    static undef2Nan(value) {
      return typeof value === "undefined" ? NaN : value;
    }
    static verViolinRange(scale, catIndex, catCount) {
      const min3 = _ScatterPlot.undef2Nan(scale(catIndex + 0.5 / SpConst.CAT_RATIO));
      const rangeLength = Math.abs(scale.range()[1] - scale.range()[0]);
      return [min3, min3 + rangeLength / catCount / 3];
    }
    static horViolinRange(scale, catIndex, catCount) {
      const max3 = _ScatterPlot.undef2Nan(scale(catIndex + 0.5 / SpConst.CAT_RATIO));
      const rangeLength = Math.abs(scale.range()[1] - scale.range()[0]);
      return [max3, max3 - rangeLength / catCount / 3];
    }
    changeMouseMode(mouseMode) {
      this.mouseMode = mouseMode;
      if (this.xDistribPlot) {
        this.xDistribPlot.mouseMode = mouseMode;
      }
      if (this.yDistribPlot) {
        this.yDistribPlot.mouseMode = mouseMode;
      }
      this.verViolinPlots.forEach(function(violinPlot) {
        violinPlot.mouseMode = mouseMode;
      });
      this.horViolinPlots.forEach(function(violinPlot) {
        violinPlot.mouseMode = mouseMode;
      });
    }
    canvasSelector(picking) {
      return this.bindto + " .canvas" + this.index + (picking ? ".picking" : ".drawing");
    }
    drawCanvas(picking) {
      this.pickingReady = picking;
      const canvasSelector = this.canvasSelector(picking);
      let canvas = select_default2(canvasSelector);
      if (canvas.empty()) {
        canvas = select_default2(this.bindto + " .MultiPlot .canvasGroup" + this.index + (picking ? ".picking" : ".drawing") + " div").append("xhtml:canvas").attr("class", "canvas" + this.index + (picking ? " picking" : " drawing"));
        if (picking) {
          canvas.style("display", "none");
        }
      }
      const xScaleRange = this.xScale.range();
      const yScaleRange = this.yScale.range();
      canvas.attr("width", xScaleRange[1] - xScaleRange[0]).attr("height", yScaleRange[0] - yScaleRange[1]);
      const canvasNode = canvas.node();
      if (!canvasNode) {
        console.error("canvasNode is null");
        return;
      }
      const context2d = canvasNode.getContext("2d", { willReadFrequently: true });
      if (!context2d) {
        console.error("context2d is null");
        return;
      }
      context2d.clearRect(0, 0, this.width, this.height);
      new PointsPlot(this).drawCanvas(picking);
    }
    codeAt(coords) {
      if (!this.pickingReady) {
        this.drawCanvas(true);
      }
      const canvasSelector = this.canvasSelector(true);
      const canvasNode = select_default2(canvasSelector).node();
      if (!canvasNode) {
        console.error("canvasNode is null for:", canvasSelector);
        return void 0;
      }
      const context2d = canvasNode.getContext("2d", { willReadFrequently: true });
      if (!context2d) {
        console.error("context2d is null");
        return void 0;
      }
      const xScaleRange = this.xScale.range();
      const yScaleRange = this.yScale.range();
      const xPlot = -xScaleRange[0];
      const yPlot = -yScaleRange[1];
      const color2 = context2d.getImageData(xPlot + coords[0], yPlot + coords[1], 1, 1).data;
      const code = color2[2] + (color2[1] << 8) + (color2[0] << 16);
      if (color2[3] !== 0 && code < this.spData.sampleData.length) {
        return code;
      }
      return void 0;
    }
    canvasMousemove(coords) {
      const code = this.codeAt(coords);
      if (typeof code === "undefined") {
        this.canvasMouseout();
      } else if (code < this.spData.sampleData.length) {
        this.spData.changeHlPoint(code, this, true);
        this.spData.sendHlPointEvent(code);
      }
    }
    canvasMouseout() {
      this.spData.changeHlPoint(null, this, true);
      this.spData.sendHlPointEvent(null);
    }
    canvasClick(coords) {
      const code = this.codeAt(coords);
      if (typeof code !== "undefined" && code < this.spData.sampleData.length) {
        this.spData.sendClickEvent(code);
      }
    }
  };

  // src/Typescript/diagPlot.ts
  var DiagPlot = class _DiagPlot {
    constructor(spData, config) {
      this.xPlot = 0;
      this.yPlot = 0;
      this.width = 0;
      this.height = 0;
      this.xScale = linear2();
      this.axisVisibility = { xTitle: true, xValues: true, yTitle: true, yValues: true };
      this.xAxis = axisBottom(this.xScale).tickFormat(_DiagPlot.prototype.formatXValue.bind(this));
      this.yScale = linear2();
      this.yAxis = axisLeft(this.yScale).tickFormat(ExpFormat.format);
      // eslint-disable-next-line no-use-before-define
      this.brush = null;
      this.dblClickTimeout = null;
      this.distribPlot = null;
      this.spData = spData;
      this.bindto = config.bindto;
      this.index = config.index;
      this.xColumn = spData.columns[spData.dimensions[0]];
      this.zColumn = spData.columns[spData.dimensions[0]];
      this.row = config.row;
      this.col = config.col;
      this.mouseMode = config.mouseMode;
      this.continuousCsId = config.continuousCsId;
      this.categoricalCsId = config.categoricalCsId;
      this.distribType = config.distribType;
      this.axisVisibility = config.axisVisibility;
      this.contColorScale = sequential(SpConst.CONTINUOUS_CS[this.continuousCsId]);
      this.catColorScale = SpConst.CATEGORIAL_CS[this.categoricalCsId];
      this.style = config.style;
    }
    static {
      this.margin = { l: 60, r: 10, b: 50, t: 5 };
    }
    static {
      this.cslRight = 30;
    }
    static {
      this.cslLeft = 10;
    }
    static {
      this.cslWidth = 20;
    }
    static {
      this.cslTotalWidth = _DiagPlot.cslRight + _DiagPlot.cslLeft + _DiagPlot.cslWidth;
    }
    static {
      this.INIT = 1;
    }
    static {
      this.SHAPE = 1 << 1;
    }
    static {
      this.PALETTE = 1 << 2;
    }
    static {
      this.Z_AXIS = 1 << 3;
    }
    static {
      this.RANGE = 1 << 4;
    }
    static {
      this.DOMAIN = 1 << 5;
    }
    setXColumn(column) {
      this.xColumn = column;
      this.distribPlot = null;
    }
    setZColumn(column) {
      this.zColumn = column;
    }
    formatXValue(value) {
      return this.xColumn.formatedValue(value);
    }
    formatZValue(value) {
      return this.zColumn === null ? "No Z axis" : this.zColumn.formatedValue(value);
    }
    draw(updateType) {
      this.xScale.range([0, this.width - ScatterPlot.padding.r]);
      this.yScale.range([this.height, ScatterPlot.padding.t]);
      this.updateColorScales();
      const plotSelection = this.plotSelection();
      this.drawSpRect(updateType, plotSelection);
      this.drawDistribPlots(updateType, plotSelection);
      this.updateXScaleDomain();
      this.updateYScaleDomain();
      this.drawXAxis(updateType, plotSelection);
      this.drawYAxis(updateType, plotSelection);
      this.drawBrush(updateType, plotSelection);
    }
    drawSpRect(updateType, plotSelection) {
      if (updateType & _DiagPlot.INIT) {
        const xScaleRange = this.xScale.range();
        const yScaleRange = this.yScale.range();
        const spRect = plotSelection.select(".spArea").append("rect").attr("class", "spRect").attr("x", xScaleRange[0]).attr("y", yScaleRange[1]).attr("width", xScaleRange[1] - xScaleRange[0]).attr("height", yScaleRange[0] - yScaleRange[1]);
        const mspDivNode = select_default2(this.bindto + " .mspDiv").node();
        const parentBounds = mspDivNode === null ? null : mspDivNode.getBoundingClientRect();
        const xParent = parentBounds === null ? 0 : parentBounds.x;
        const yParent = parentBounds === null ? 0 : parentBounds.y;
        const spRectNode = spRect.node();
        const spRectBounds = spRectNode === null ? null : spRectNode.getBoundingClientRect();
        const xSpRect = spRectBounds === null ? 0 : spRectBounds.x;
        const ySpRect = spRectBounds === null ? 0 : spRectBounds.y;
        this.xPlot = xSpRect - xParent;
        this.yPlot = ySpRect - yParent;
      }
    }
    hlGraph(highlight) {
      const plotSelection = this.plotSelection();
      plotSelection.select(".spRect").classed("hlGraph", highlight);
    }
    // eslint-disable-next-line max-lines-per-function
    drawDistribPlots(updateType, plotSelection) {
      const distribGroup = plotSelection.select(".distribGroup");
      if (updateType & _DiagPlot.INIT || !this.distribPlot) {
        this.distribPlot = new DistributionPlot(
          this.spData,
          this.xColumn,
          {
            bindto: this.bindto,
            orientation: DistributionPlot.HOR,
            mouseMode: this.mouseMode,
            categoricalCsId: this.categoricalCsId,
            distribType: this.distribType,
            style: this.style
          },
          this.index,
          this.xPlot,
          this.yPlot
        );
        this.distribPlot.generate(distribGroup, `${this.bindto}-tile-clip`);
      }
      if (updateType & (_DiagPlot.INIT | _DiagPlot.SHAPE | _DiagPlot.RANGE | _DiagPlot.DOMAIN | _DiagPlot.Z_AXIS)) {
        this.distribPlot.valuesScaleRange(this.xScale.range()).computePlot(this.zColumn).distribScaleRange(this.yScale.range());
      }
      if (updateType & (_DiagPlot.INIT | _DiagPlot.PALETTE | _DiagPlot.Z_AXIS)) {
        this.distribPlot.colorScale(this.catColorScale);
      }
      this.distribPlot.update(updateType, distribGroup);
    }
    updateXScaleDomain() {
      this.xScale.range([0, this.width - ScatterPlot.padding.r]).domain(this.xColumn.domain());
      if (this.xColumn.categories === null) {
        this.xScale.nice();
      }
      this.xAxis.scale(this.xScale).ticks(this.xColumn.axisTicks()).tickSize(-this.height + ScatterPlot.padding.t);
    }
    updateYScaleDomain() {
      if (this.distribPlot === null) {
        console.error("updateYScaleDomain, 'distribPlot' is null");
      } else {
        this.yScale.domain(
          this.distribPlot.useHistogramRep() ? this.distribPlot.mainDistrib.cutHistoScale.domain() : this.distribPlot.mainDistrib.cutDensityScale.domain()
        ).nice();
        this.yAxis.scale(this.yScale).ticks(5).tickSize(-this.width + ScatterPlot.padding.r);
      }
    }
    updateColorScales() {
      const zColumn = this.zColumn;
      if (zColumn === null) {
        return;
      }
      if (zColumn.categories === null) {
        const [zMin, zMax] = zColumn.domain();
        this.contColorScale = sequential(SpConst.CONTINUOUS_CS[this.continuousCsId]).domain([zMin, zMax]);
      } else {
        const zMax = zColumn.domain()[1];
        this.catColorScale = SpConst.CATEGORIAL_CS[this.categoricalCsId].domain(range(zMax));
      }
    }
    drawXAxis(updateType, plotSelection) {
      if (updateType & (_DiagPlot.INIT | _DiagPlot.RANGE | _DiagPlot.DOMAIN | _DiagPlot.SHAPE)) {
        const axesGroup = plotSelection.select(".axesGroup");
        if (updateType & _DiagPlot.INIT) {
          axesGroup.append("g").attr("class", "x axis").attr("transform", "translate(0," + this.height + ")");
          if (this.axisVisibility.xTitle) {
            const x2 = (this.width - ScatterPlot.padding.r) / 2;
            const y2 = this.height + _DiagPlot.margin.b / 2;
            axesGroup.append("text").attr("class", "x scatterlabel").attr("x", x2).attr("y", y2).attr("text-anchor", "middle").attr("dominant-baseline", "middle");
          }
        }
        axesGroup.select(".x.axis").call(this.xAxis).selectAll(".tick text").attr("transform", "rotate(45)").style("text-anchor", "start").attr("display", this.axisVisibility.xValues ? "block" : "none");
        if (this.axisVisibility.xTitle) {
          axesGroup.select(".x.scatterlabel").text(this.xColumn.label.replace(/<br>/gi, " "));
        }
      }
    }
    drawYAxis(updateType, plotSelection) {
      if (updateType & (_DiagPlot.INIT | _DiagPlot.RANGE | _DiagPlot.DOMAIN | _DiagPlot.SHAPE)) {
        const axesGroup = plotSelection.select(".axesGroup");
        if (updateType & _DiagPlot.INIT) {
          axesGroup.append("g").attr("class", "y axis");
          if (this.axisVisibility.xTitle) {
            const x2 = -_DiagPlot.margin.l * 0.7;
            const y2 = (this.height + ScatterPlot.padding.t) / 2;
            axesGroup.append("text").attr("class", "y scatterlabel").attr("transform", "translate(" + x2 + "," + y2 + ")rotate(270)").attr("dominant-baseline", "baseline").attr("text-anchor", "middle");
          }
        }
        axesGroup.select(".y.axis").call(this.yAxis).selectAll(".tick text").attr("display", this.axisVisibility.yValues ? "block" : "none");
        if (this.axisVisibility.yTitle) {
          axesGroup.select(".y.scatterlabel").text(this.xColumn.label.replace(/<br>/gi, " "));
        }
      }
    }
    fixBrush() {
      const plotSelection = this.plotSelection();
      if (this.mouseMode === SpConst.tooltipMouse.key) {
        plotSelection.selectAll(".selection").style("display", "none");
        plotSelection.selectAll(".handle").style("display", "none");
        plotSelection.selectAll(".overlay").style("pointer-events", "auto");
      } else {
        plotSelection.selectAll(".selection").style("display", null);
        plotSelection.selectAll(".handle").style("display", null);
        plotSelection.selectAll(".overlay").style("pointer-events", "all");
      }
      if (this.mouseMode === SpConst.filterMouse.key) {
        this.drawBrush(ScatterPlot.RANGE, plotSelection);
      }
    }
    drawBrush(updateType, plotSelection) {
      const spArea = plotSelection.select(".spArea");
      if (updateType & _DiagPlot.INIT || !this.brush) {
        this.brush = brush_default().on("end", (event) => {
          const brushZone = event.selection;
          this.brushend(brushZone);
        });
        const xExtent = [
          this.xScale.range()[0],
          this.yScale.range()[1]
        ];
        const yExtent = [
          this.xScale.range()[1],
          this.yScale.range()[0]
        ];
        this.brush.extent([xExtent, yExtent]);
        spArea.call(this.brush);
      }
    }
    // eslint-disable-next-line max-lines-per-function
    brushend(brushZone) {
      if (!this.brush) {
        return;
      }
      const thisPlot = this;
      if (this.mouseMode === SpConst.zoomMouse.key) {
        if (!brushZone && !this.dblClickTimeout) {
          this.dblClickTimeout = setTimeout(function() {
            thisPlot.dblClickTimeout = null;
          }, SpConst.dblClickDelay);
          return;
        }
        const plotSelection = this.plotSelection();
        if (brushZone) {
          this.xScale.domain([brushZone[0][0], brushZone[1][0]].map(this.xScale.invert));
          this.yScale.domain([brushZone[1][1], brushZone[0][1]].map(this.yScale.invert));
          plotSelection.select(".spArea").call(this.brush.clear);
        } else {
          this.xScale.domain(this.xColumn.domain());
          if (this.distribPlot) {
            this.yScale.domain(this.distribPlot.mainDistrib.cutDensityScale.domain());
          }
        }
        if (this.xColumn.categories === null) {
          this.xScale.nice();
        }
        this.drawXAxis(_DiagPlot.DOMAIN, plotSelection);
        this.drawYAxis(_DiagPlot.DOMAIN, plotSelection);
        this.drawBrush(_DiagPlot.DOMAIN, plotSelection);
        if (this.distribPlot) {
          this.distribPlot.valuesScale.domain(this.xScale.domain());
          this.distribPlot.update(_DiagPlot.DOMAIN, plotSelection.select(".distribGroup"));
        }
      }
    }
    plotSelection(plotSelection) {
      if (plotSelection) {
        return plotSelection;
      }
      const thisPlot = this;
      const mspGroup = select_default2(this.bindto + " .mspGroup");
      return mspGroup.selectAll(".diagPlot").filter(function(plot) {
        return plot.row === thisPlot.row && plot.col === thisPlot.col;
      });
    }
    distribRepChange(newType) {
      this.distribType = newType;
      if (this.distribPlot) {
        this.distribPlot.distribType = newType;
      }
      const plotSelection = this.plotSelection();
      this.drawDistribPlots(_DiagPlot.SHAPE, plotSelection);
      this.updateYScaleDomain();
      this.drawYAxis(_DiagPlot.SHAPE, plotSelection);
    }
    changeMouseMode(mouseMode) {
      this.mouseMode = mouseMode;
      if (this.distribPlot) {
        this.distribPlot.mouseMode = mouseMode;
      }
    }
  };

  // src/Typescript/style.ts
  var Style = class _Style {
    static defaultPlotProperties() {
      return {
        /** Color used when categories coloring is not applied */
        noCatColor: "#43665e",
        /** Color used to show in background full plots, with cutoffs */
        watermarkColor: "#e4e4e4",
        point: {
          /** Radius used to draw points as circles */
          radius: 2,
          /** Opacity value used for points */
          alpha: 0.5
        },
        regression: {
          /** Width of path stroke */
          strokeWidth: 4
        }
      };
    }
    constructor(bindto) {
      this.bindto = bindto;
      this.plotProperties = _Style.defaultPlotProperties();
    }
    initWith(cssRules, plotProperties) {
      this.cssRules = cssRules;
      this.plotProperties = _Style.defaultPlotProperties();
      if (plotProperties) {
        if (typeof plotProperties.noCatColor !== "undefined") {
          if (_Style.isValidColor(plotProperties.noCatColor)) {
            this.plotProperties.noCatColor = plotProperties.noCatColor;
          } else {
            console.error(`plotProperties.noCatColor ${plotProperties.noCatColor} is invalid`);
          }
        }
        if (typeof plotProperties.watermarkColor !== "undefined") {
          if (_Style.isValidColor(plotProperties.watermarkColor)) {
            this.plotProperties.watermarkColor = plotProperties.watermarkColor;
          } else {
            console.error(`plotProperties.watermarkColor ${plotProperties.watermarkColor} is invalid`);
          }
        }
      }
      if (plotProperties && plotProperties.point) {
        this.initPointPlotPropertiesWith(plotProperties.point);
      }
      if (plotProperties && plotProperties.regression) {
        this.initRegressionPlotPropertiesWith(plotProperties.regression);
      }
    }
    initPointPlotPropertiesWith(pointPlotProperties) {
      if (typeof pointPlotProperties.alpha !== "undefined") {
        if (_Style.isValidAlpha(pointPlotProperties.alpha)) {
          this.plotProperties.point.alpha = +pointPlotProperties.alpha;
        } else {
          console.error(`plotProperties.alpha ${pointPlotProperties.alpha} is invalid`);
        }
      }
      if (typeof pointPlotProperties.radius !== "undefined") {
        if (_Style.isPositiveNumber(pointPlotProperties.radius)) {
          this.plotProperties.point.radius = +pointPlotProperties.radius;
        } else {
          console.error(`plotProperties.point.radius ${pointPlotProperties.radius} is invalid`);
        }
      }
    }
    initRegressionPlotPropertiesWith(regressionPlotProperties) {
      if (typeof regressionPlotProperties.strokeWidth !== "undefined") {
        if (_Style.isPositiveNumber(regressionPlotProperties.strokeWidth)) {
          this.plotProperties.regression.strokeWidth = +regressionPlotProperties.strokeWidth;
        } else {
          console.error(`plotProperties.regression.strokeWidth ${regressionPlotProperties.strokeWidth} is invalid`);
        }
      }
    }
    static isValidColor(colorSpecifier) {
      return colorSpecifier !== null && color(colorSpecifier) !== null;
    }
    static isValidAlpha(value) {
      if (value) {
        const valueAsNumber = +value;
        return valueAsNumber >= 0 && valueAsNumber <= 1 && valueAsNumber.toString(10) === value.toString(10);
      }
      return false;
    }
    static isPositiveNumber(value) {
      if (value) {
        const valueAsNumber = +value;
        return valueAsNumber >= 0 && valueAsNumber.toString(10) === value.toString(10);
      }
      return false;
    }
    applyCssRules() {
      if (this.cssRules) {
        for (const [selector, declarations] of Object.entries(this.cssRules)) {
          const selection2 = select_default2(this.bindto).selectAll(selector);
          const applyDeclaration = (declaration) => {
            const splitted = declaration.split(":");
            if (splitted.length === 2) {
              selection2.style(splitted[0], splitted[1]);
            } else {
              console.error("Invalid CSS declaration:", declaration);
            }
          };
          if (Array.isArray(declarations)) {
            declarations.forEach(applyDeclaration);
          }
          if (typeof declarations === "string") {
            applyDeclaration(declarations);
          }
        }
      }
    }
  };

  // src/Typescript/scatterPlotMatrix.ts
  var ScatterPlotMatrix = class _ScatterPlotMatrix {
    constructor(id2, width, height) {
      this.zColumn = null;
      this.size = 900;
      this.mouseMode = SpConst.tooltipMouse.key;
      this.rotateTitle = false;
      this.distribType = // DistributionPlot.DENS_REP;
      // DistributionPlot.DENS_REP |
      DistributionPlot.HISTO_REP;
      this.regressionType = 0;
      // RegressionPlot.LOESS_REP;
      // RegressionPlot.LOESS_REP |
      // RegressionPlot.LINEAR_REP;
      this.corrPlotType = // CorrPlot.EMPTY;
      // CorrPlot.TEXT;
      // CorrPlot.ABS_TEXT_REP;
      CorrPlot.CIRCLES_REP;
      this.continuousCsId = SpConst.CONTINUOUS_CS_IDS[0];
      this.categoricalCsId = SpConst.CATEGORIAL_CS_IDS[0];
      this.corrPlotCsId = SpConst.CONTINUOUS_CS_IDS[0];
      this.scatterPlotList = [];
      this.diagPlotList = [];
      this.corrPlotList = [];
      this.brushSlidersLinked = true;
      this.defaultVisibleDimCount = 0;
      this.visibleDimCount = 0;
      this.xStartingDimIndex = 0;
      this.yStartingDimIndex = 0;
      this.useControlWidgets = false;
      this.bindto = "#" + id2;
      this.style = new Style(this.bindto);
      this.setSize(width, height);
    }
    static {
      this.margin = { t: 95, r: 95, b: 95, l: 95 };
    }
    static {
      this.zAxisClass = "ZAxis";
    }
    static {
      this.PLOT_EVENT = SpData.PLOT_EVENT;
    }
    static {
      this.ZAXIS_EVENT = "zAxisChange";
    }
    static {
      this.CUTOFF_EVENT = SpData.CUTOFF_EVENT;
    }
    static {
      this.HL_POINT_EVENT = SpData.HL_POINT_EVENT;
    }
    static {
      this.POINT_CLICK_EVENT = SpData.POINT_CLICK_EVENT;
    }
    static {
      this.MAX_VISIBLE_DIMS = 16;
    }
    static {
      this.DEFAULT_SLIDERS_POSITION = {
        dimCount: 8,
        xStartingDimIndex: 0,
        yStartingDimIndex: 0
      };
    }
    resize(width, height) {
      this.setSize(width, height);
      select_default2(this.bindto + " .MultiPlot svg").attr("width", this.getSize() + _ScatterPlotMatrix.margin.l + _ScatterPlotMatrix.margin.r).attr("height", this.getSize() + _ScatterPlotMatrix.margin.b + _ScatterPlotMatrix.margin.t);
      this.removePlots();
      this.updatePlots(ScatterPlot.INIT);
      this.xBrushSlider.update();
      this.yBrushSlider.update();
    }
    setSize(width, height) {
      const wSize = width - (_ScatterPlotMatrix.margin.l + _ScatterPlotMatrix.margin.r);
      const hSize = height - (_ScatterPlotMatrix.margin.t + _ScatterPlotMatrix.margin.b);
      this.size = Math.max(200, Math.min(wSize, hSize));
    }
    getSize() {
      if (this.useControlWidgets) {
        const controlDivNode = select_default2(this.bindto + " .controlDiv").node();
        if (controlDivNode !== null) {
          const controlDivHeight = controlDivNode.getBoundingClientRect().height;
          if (this.size > controlDivHeight) {
            return this.size - controlDivHeight;
          }
        }
      }
      return this.size;
    }
    removePlots() {
      this.scatterPlotList.forEach((plot) => {
        plot.removePlot();
      });
      selectAll_default2(this.bindto + " .diagPlot").remove();
      selectAll_default2(this.bindto + " .corrPlot").remove();
    }
    id() {
      return this.bindto.substring(1);
    }
    initSlidersPosition(config) {
      if (config.slidersPosition) {
        if (typeof config.slidersPosition.dimCount !== "number" || config.slidersPosition.dimCount > _ScatterPlotMatrix.MAX_VISIBLE_DIMS) {
          this.defaultVisibleDimCount = _ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.dimCount;
        } else {
          this.defaultVisibleDimCount = config.slidersPosition.dimCount;
        }
        if (typeof config.slidersPosition.xStartingDimIndex === "number") {
          this.xStartingDimIndex = config.slidersPosition.xStartingDimIndex;
        } else {
          this.xStartingDimIndex = _ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.xStartingDimIndex;
        }
        if (typeof config.slidersPosition.yStartingDimIndex === "number") {
          this.yStartingDimIndex = config.slidersPosition.yStartingDimIndex;
        } else {
          this.yStartingDimIndex = _ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.yStartingDimIndex;
        }
      } else {
        this.defaultVisibleDimCount = _ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.dimCount;
        this.xStartingDimIndex = _ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.xStartingDimIndex;
        this.yStartingDimIndex = _ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.yStartingDimIndex;
      }
      if (this.spData.dimensions.length < this.defaultVisibleDimCount) {
        this.visibleDimCount = this.spData.dimensions.length;
      } else {
        this.visibleDimCount = this.defaultVisibleDimCount;
      }
      if (this.xStartingDimIndex > this.spData.dimensions.length - this.visibleDimCount) {
        this.xStartingDimIndex = this.spData.dimensions.length - this.visibleDimCount;
      }
      if (this.yStartingDimIndex > this.spData.dimensions.length - this.visibleDimCount) {
        this.yStartingDimIndex = this.spData.dimensions.length - this.visibleDimCount;
      }
      this.brushSlidersLinked = this.xStartingDimIndex === this.yStartingDimIndex;
    }
    generate(config) {
      if (select_default2(this.bindto).empty()) {
        throw new Error("'bindto' dom element not found:" + this.bindto);
      }
      this.zColumn = null;
      this.style.initWith(config.cssRules, config.plotProperties);
      this.initData(config);
      this.initSlidersPosition(config);
      this.initTilePlots();
      this.setZAxis(config.zAxisDim, true);
      this.useControlWidgets = config.controlWidgets ? config.controlWidgets : false;
      this.buildMainDomElements();
      this.appendPlotSvg();
      this.spData.on(SpData.HL_GRAPH_EVENT, _ScatterPlotMatrix.prototype.hlGraph.bind(this));
      this.spData.on(SpData.MOUSE_OVER_POINT_EVENT, _ScatterPlotMatrix.prototype.mouseOverEvent.bind(this));
      this.updatePlots(ScatterPlot.INIT);
      this.xBrushSlider.update();
      this.yBrushSlider.update();
      return this;
    }
    hlGraph(targetPlot) {
      this.scatterPlotList.forEach((plot) => {
        plot.hlGraph(false);
      });
      this.diagPlotList.forEach((plot) => {
        plot.hlGraph(false);
      });
      this.corrPlotList.forEach((plot) => {
        plot.hlGraph(false);
      });
      if (targetPlot !== null) {
        const thisMPlot = this;
        const targetPlotXDimIndex = thisMPlot.xStartingDimIndex + targetPlot.col;
        const targetPlotYDimIndex = thisMPlot.yStartingDimIndex + targetPlot.row;
        [
          [targetPlotXDimIndex, targetPlotXDimIndex],
          [targetPlotXDimIndex, targetPlotYDimIndex],
          [targetPlotYDimIndex, targetPlotYDimIndex],
          [targetPlotYDimIndex, targetPlotXDimIndex]
        ].forEach(function(plotCoord) {
          thisMPlot.scatterPlotList.forEach((plot) => {
            if (thisMPlot.xStartingDimIndex + plot.col === plotCoord[0] && thisMPlot.yStartingDimIndex + plot.row === plotCoord[1]) {
              plot.hlGraph(true);
            }
          });
          thisMPlot.diagPlotList.forEach((plot) => {
            if (thisMPlot.xStartingDimIndex + plot.col === plotCoord[0] && thisMPlot.yStartingDimIndex + plot.row === plotCoord[1]) {
              plot.hlGraph(true);
            }
          });
          thisMPlot.corrPlotList.forEach((plot) => {
            if (thisMPlot.xStartingDimIndex + plot.col === plotCoord[0] && thisMPlot.yStartingDimIndex + plot.row === plotCoord[1]) {
              plot.hlGraph(true);
            }
          });
        });
      }
    }
    mouseOverEvent(hlEvent) {
      if (hlEvent.pointIndex === null) {
        new PointsPlot(hlEvent.scatterPlot).mouseout();
      } else {
        new PointsPlot(hlEvent.scatterPlot).mouseover(this.spData.sampleData[hlEvent.pointIndex], hlEvent.pointIndex, this.scatterPlotList);
      }
    }
    on(type2, callback) {
      this.spData.dispatch.on(type2, callback);
    }
    // eslint-disable-next-line max-lines-per-function
    initData(config) {
      if (!config.continuousCS) {
        this.continuousCsId = SpConst.CONTINUOUS_CS_IDS[0];
      } else if (SpConst.CONTINUOUS_CS_IDS.includes(config.continuousCS)) {
        this.continuousCsId = config.continuousCS;
      } else {
        console.error("Unknown continuous color scale: " + config.continuousCS);
      }
      if (!config.categoricalCS) {
        this.categoricalCsId = SpConst.CATEGORIAL_CS_IDS[0];
      } else if (SpConst.CATEGORIAL_CS_IDS.includes(config.categoricalCS)) {
        this.categoricalCsId = config.categoricalCS;
      } else {
        console.error("Unknown categorical color scale: " + config.categoricalCS);
      }
      if (!config.mouseMode) {
        this.mouseMode = SpConst.mouseModeList[0].key;
      } else if (SpConst.mouseModeList.findIndex((mode) => mode.key === config.mouseMode) === -1) {
        console.error("Invalid mouse mode: " + config.mouseMode);
      } else {
        this.mouseMode = config.mouseMode;
      }
      if (!config.distribType) {
        this.distribType = 2;
      } else if ([0, 1, 2, 3].includes(config.distribType)) {
        this.distribType = config.distribType;
      } else {
        console.error("Unknown distribType: " + config.distribType);
      }
      if (!config.regressionType) {
        this.regressionType = 0;
      } else if ([0, 1, 2, 3].includes(config.regressionType)) {
        this.regressionType = config.regressionType;
      } else {
        console.error("Unknown regressionType: " + config.regressionType);
      }
      if (!config.corrPlotType) {
        this.corrPlotType = CorrPlot.CIRCLES_REP;
      } else if ([CorrPlot.EMPTY_REP, CorrPlot.CIRCLES_REP, CorrPlot.TEXT_REP, CorrPlot.ABS_TEXT_REP].includes(config.corrPlotType)) {
        this.corrPlotType = config.corrPlotType;
      } else {
        console.error("Unknown correlation plot type: " + config.corrPlotType);
      }
      if (this.corrPlotType === CorrPlot.TEXT_REP) {
        this.corrPlotCsId = SpConst.CONTINUOUS_CS_IDS[21];
      } else if (this.corrPlotType === CorrPlot.ABS_TEXT_REP) {
        this.corrPlotCsId = SpConst.CONTINUOUS_CS_IDS[8];
      }
      if (config.corrPlotCS) {
        if (SpConst.CONTINUOUS_CS_IDS.includes(config.corrPlotCS)) {
          this.corrPlotCsId = config.corrPlotCS;
        } else {
          console.error("Unknown correlation color scale: " + config.corrPlotCS);
        }
      }
      this.rotateTitle = config.rotateTitle ? config.rotateTitle : false;
      this.spData = new SpData(config);
      this.spData.updateCutRowsAttributes();
      this.spData.on(SpData.ROW_FILTER_EVENT, _ScatterPlotMatrix.prototype.rowFilterChange.bind(this));
    }
    adjustVisibleDimensions() {
      if (this.spData.dimensions.length < this.defaultVisibleDimCount) {
        this.visibleDimCount = this.spData.dimensions.length;
      } else {
        this.visibleDimCount = this.defaultVisibleDimCount;
      }
      this.xStartingDimIndex = 0;
      this.yStartingDimIndex = 0;
    }
    initTilePlots() {
      this.scatterPlotList.splice(0, this.scatterPlotList.length);
      this.diagPlotList.splice(0, this.diagPlotList.length);
      this.corrPlotList.splice(0, this.corrPlotList.length);
      for (let j = 0; j < this.visibleDimCount; j++) {
        for (let i = 0; i < this.visibleDimCount; i++) {
          const xVisibleDimIndex = this.xStartingDimIndex + i;
          const yVisibleDimIndex = this.yStartingDimIndex + j;
          if (xVisibleDimIndex < yVisibleDimIndex) {
            this.pushNewSP(i, j);
          }
          if (xVisibleDimIndex === yVisibleDimIndex) {
            this.pushNewDP(i, j);
          }
          if (xVisibleDimIndex > yVisibleDimIndex) {
            this.pushNewCP(i, j);
          }
        }
      }
    }
    rowFilterChange() {
      this.spData.updateCutRowsAttributes();
      this.scatterPlotList.forEach((plot) => {
        const plotSelection = plot.plotSelection();
        plot.drawRegressionPlots(ScatterPlot.DOMAIN, plotSelection);
        plot.drawDistribPlots(ScatterPlot.DOMAIN, plotSelection);
        plot.drawVerViolinPlots(ScatterPlot.DOMAIN, plotSelection);
        plot.drawHorViolinPlots(ScatterPlot.DOMAIN, plotSelection);
        if (plotSelection.size() !== 0) {
          plot.drawCanvas(false);
        }
      });
      this.diagPlotList.forEach((plot) => {
        const plotSelection = plot.plotSelection();
        plot.drawDistribPlots(DiagPlot.DOMAIN, plotSelection);
        plot.updateYScaleDomain();
        plot.drawYAxis(DiagPlot.DOMAIN, plotSelection);
      });
      this.corrPlotList.forEach((plot) => {
        plot.draw(ScatterPlot.DOMAIN);
      });
    }
    pushNewSP(i, j) {
      const scatterPlot = new ScatterPlot(this.spData, {
        bindto: this.bindto,
        index: i + this.visibleDimCount * j,
        row: j,
        col: i,
        regressionType: this.regressionType,
        mouseMode: this.mouseMode,
        continuousCsId: this.continuousCsId,
        categoricalCsId: this.categoricalCsId,
        distribVisibility: false,
        distribType: this.distribType,
        corrPlotType: this.corrPlotType,
        corrPlotCsId: this.corrPlotCsId,
        axisVisibility: {
          xTitle: false,
          xValues: j === this.visibleDimCount - 1,
          yTitle: false,
          yValues: i === 0
        },
        style: this.style
      });
      scatterPlot.setXColumn(this.spData.columns[this.spData.dimensions[this.xStartingDimIndex + i]]);
      scatterPlot.setYColumn(this.spData.columns[this.spData.dimensions[this.yStartingDimIndex + j]]);
      scatterPlot.setZColumn(this.getZColumn());
      this.scatterPlotList.push(scatterPlot);
    }
    pushNewDP(i, j) {
      const diagPlot = new DiagPlot(this.spData, {
        bindto: this.bindto,
        index: i + this.visibleDimCount * j,
        row: j,
        col: i,
        regressionType: this.regressionType,
        mouseMode: this.mouseMode,
        continuousCsId: this.continuousCsId,
        categoricalCsId: this.categoricalCsId,
        distribVisibility: false,
        distribType: this.distribType,
        corrPlotType: this.corrPlotType,
        corrPlotCsId: this.corrPlotCsId,
        axisVisibility: {
          xTitle: false,
          xValues: j === this.visibleDimCount - 1,
          yTitle: false,
          yValues: i === 0
        },
        style: this.style
      });
      diagPlot.setXColumn(this.spData.columns[this.spData.dimensions[this.xStartingDimIndex + i]]);
      diagPlot.setZColumn(this.getZColumn());
      this.diagPlotList.push(diagPlot);
    }
    pushNewCP(i, j) {
      const corrPlot = new CorrPlot(this.spData, {
        bindto: this.bindto,
        index: i + this.visibleDimCount * j,
        row: j,
        col: i,
        regressionType: this.regressionType,
        mouseMode: this.mouseMode,
        continuousCsId: this.continuousCsId,
        categoricalCsId: this.categoricalCsId,
        distribVisibility: false,
        distribType: this.distribType,
        corrPlotType: this.corrPlotType,
        corrPlotCsId: this.corrPlotCsId,
        axisVisibility: {
          xTitle: true,
          xValues: true,
          yTitle: true,
          yValues: true
        },
        style: this.style
      });
      corrPlot.setXColumn(this.spData.columns[this.spData.dimensions[this.xStartingDimIndex + i]]);
      corrPlot.setYColumn(this.spData.columns[this.spData.dimensions[this.yStartingDimIndex + j]]);
      corrPlot.setZColumn(this.getZColumn());
      this.corrPlotList.push(corrPlot);
    }
    getZColumn() {
      if (this.zColumn) {
        return this.zColumn;
      }
      return null;
    }
    // eslint-disable-next-line max-lines-per-function
    buildMainDomElements() {
      select_default2(this.bindto + " .mspDiv").remove();
      const mspDiv = select_default2(this.bindto).append("div").attr("class", "mspDiv").classed("withWidgets", this.useControlWidgets).classed("withoutWidgets", !this.useControlWidgets);
      const controlDiv = mspDiv.append("div").attr("class", "controlDiv");
      const optionalPlotsDiv = controlDiv.append("div").attr("class", "optionalPlotsDiv");
      optionalPlotsDiv.append("div").attr("class", "distribRepDiv").html('Distribution Representation: <span class="distribRepSelect"></span>');
      optionalPlotsDiv.append("div").attr("class", "linearRegrDiv").html(`<input type="checkbox" id="${this.id()}_linearRegr" name="linearRegr"> <label for="${this.id()}_linearRegr">Linear Regression</label>`);
      optionalPlotsDiv.append("div").attr("class", "loessDiv").html(`<input type="checkbox" id="${this.id()}_loess" name="loess"> <label for="${this.id()}_loess">Local Polynomial Regression</label>`);
      const csDiv = controlDiv.append("div").attr("class", "csDiv");
      csDiv.append("div").attr("class", "zAxisUsedDiv").html(`<input type="checkbox" id="${this.id()}_zAxisUsed" name="zAxisUsed" checked> <label for="${this.id()}_zAxisUsed">Use Z Axis</label> <span class="ParamSelect ZAxis"></span>`);
      csDiv.append("div").attr("class", "contCsDiv").html('Continuous Color Scale: <span class="contCsSelect"></span>');
      csDiv.append("div").html('Categorical Color Scale: <span class="catCsSelect"></span>');
      const corrDiv = controlDiv.append("div").attr("class", "corrDiv");
      corrDiv.append("div").attr("class", "corrTypeDiv").html('Correlation Plot Type: <span class="corrTypeSelect"></span>');
      corrDiv.append("div").attr("class", "corrCsDiv").html('Correlation Color Scale: <span class="corrCsSelect"></span>');
      corrDiv.append("div").attr("class", "mouseModeDiv").html('Mouse mode: <span class="mouseModeSelect"></span>');
      mspDiv.append("div").attr("class", "MultiPlot");
      this.appendDistribRepSelect();
      this.appendContCsSelect();
      this.appendCatCsSelect();
      this.appendCorrTypeSelect();
      this.appendCorrCsSelect();
      this.appendZAxisSelector();
      this.initZAxisUsedCB();
      this.appendMouseModeSelect();
      this.initRegressionCB();
    }
    appendPlotSvg() {
      const mspSvg = select_default2(this.bindto + " .MultiPlot").append("svg").attr("width", this.getSize() + _ScatterPlotMatrix.margin.l + _ScatterPlotMatrix.margin.r).attr("height", this.getSize() + _ScatterPlotMatrix.margin.b + _ScatterPlotMatrix.margin.t);
      mspSvg.append("g").attr("transform", `translate(${_ScatterPlotMatrix.margin.l}, ${_ScatterPlotMatrix.margin.t})`).attr("class", "mspGroup");
      this.appendSvgDefs();
      this.appendBrushSliders();
    }
    appendSvgDefs() {
      const svg = select_default2(this.bindto + " svg");
      const defs = svg.append("defs");
      defs.append("clipPath").attr("id", `${this.id()}-tile-clip`).append("rect");
    }
    appendBrushSliders() {
      this.xBrushSlider = new BrushSlider(this, true);
      this.yBrushSlider = new BrushSlider(this, false);
    }
    updateVisibleDimensions(begin, end, xOriented) {
      if (begin !== void 0 && end !== void 0 && begin >= 0 && end >= 0) {
        if (xOriented || this.brushSlidersLinked) {
          this.xStartingDimIndex = begin;
        }
        if (!xOriented || this.brushSlidersLinked) {
          this.yStartingDimIndex = begin;
        }
        this.visibleDimCount = end - begin + 1;
        this.defaultVisibleDimCount = this.visibleDimCount;
        this.tilesNumberChanged();
      }
    }
    tilesNumberChanged() {
      this.removePlots();
      this.initTilePlots();
      this.updatePlots(ScatterPlot.INIT);
    }
    // eslint-disable-next-line max-lines-per-function
    updatePlots(updateType) {
      const thisMPlot = this;
      const mspGroup = select_default2(this.bindto + " .MultiPlot .mspGroup");
      const tileWidth = this.getSize() / thisMPlot.visibleDimCount;
      const tileHeight = this.getSize() / thisMPlot.visibleDimCount;
      if (updateType & (ScatterPlot.INIT | ScatterPlot.RANGE)) {
        this.scatterPlotList.forEach((plot) => {
          plot.height = tileHeight;
          plot.width = tileWidth;
        });
        this.diagPlotList.forEach((plot) => {
          plot.height = tileHeight;
          plot.width = tileWidth;
        });
        this.corrPlotList.forEach((plot) => {
          plot.height = tileHeight;
          plot.width = tileWidth;
        });
        select_default2(`${this.bindto}-tile-clip > rect`).attr("x", 0).attr("y", ScatterPlot.padding.t).attr("width", tileWidth - ScatterPlot.padding.r).attr("height", tileHeight - ScatterPlot.padding.t);
      }
      if (updateType & ScatterPlot.INIT) {
        this.initColHeaders();
        mspGroup.selectAll(".corrPlot").data(this.corrPlotList).enter().append("g").attr("class", "corrPlot").attr("transform", function(plot) {
          return `translate(${thisMPlot.xSubPlot(plot)}, ${thisMPlot.ySubPlot(plot)})`;
        });
        const diagPlot = mspGroup.selectAll(".diagPlot").data(this.diagPlotList).enter().append("g").attr("class", "diagPlot").attr("transform", function(plot) {
          return `translate(${thisMPlot.xSubPlot(plot)}, ${thisMPlot.ySubPlot(plot)})`;
        });
        diagPlot.append("g").attr("class", "axesGroup");
        diagPlot.append("g").attr("class", "spArea").attr("clip-path", `url(${this.bindto}-tile-clip)`);
        diagPlot.append("g").attr("class", "distribGroup");
        const scatterPlot = mspGroup.selectAll(".scatterPlot").data(this.scatterPlotList).enter().append("g").attr("class", "scatterPlot").attr("transform", function(plot) {
          return `translate(${thisMPlot.xSubPlot(plot)}, ${thisMPlot.ySubPlot(plot)})`;
        });
        scatterPlot.append("g").attr("class", "axesGroup");
        scatterPlot.append("g").attr("class", "spArea").attr("clip-path", `url(${this.bindto}-tile-clip)`);
        scatterPlot.append("g").attr("class", "distribGroup");
      }
      mspGroup.selectAll(".scatterPlot").each(function(plot) {
        plot.draw(updateType);
      });
      mspGroup.selectAll(".diagPlot").each(function(plot) {
        plot.draw(updateType);
      });
      mspGroup.selectAll(".corrPlot").each(function(plot) {
        plot.draw(updateType);
      });
      mspGroup.selectAll(".cslGroup").style("display", "none");
      if (updateType & ScatterPlot.INIT) {
        this.fixBrush();
        this.style.applyCssRules();
      }
    }
    initColHeaders() {
      this.initTopHeaders();
      this.initRightHeaders();
      this.initBottomHeaders();
      this.initLeftHeaders();
    }
    initTopHeaders() {
      const thisMPlot = this;
      const mspGroup = select_default2(this.bindto + " .MultiPlot .mspGroup");
      const tileWidth = this.getSize() / thisMPlot.visibleDimCount;
      mspGroup.selectAll(".topHeader").data(range(this.visibleDimCount)).join(
        (enter) => enter.append("text").attr("class", "topHeader Header").on("click", _ScatterPlotMatrix.prototype.clickColTitle.bind(thisMPlot)),
        (update) => update,
        (exit) => exit.remove()
      ).classed("input", function(colIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isInput();
      }).classed("output", function(colIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isOutput();
      }).classed("zAxis", function(colIndex) {
        return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]];
      }).attr("transform", function(colIndex) {
        if (thisMPlot.rotateTitle) {
          return `translate(${tileWidth * (colIndex + 0.3)}, -10)rotate(-10)`;
        } else {
          return `translate(${tileWidth * (colIndex + 0.5)}, -10)`;
        }
      }).each(function(colIndex) {
        ScatterPlot.tspanColumnTitleBT(select_default2(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]);
      }).attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
    }
    initRightHeaders() {
      const thisMPlot = this;
      const mspGroup = select_default2(this.bindto + " .MultiPlot .mspGroup");
      const tileHeight = this.getSize() / thisMPlot.visibleDimCount;
      mspGroup.selectAll(".rightHeader").data(range(this.visibleDimCount)).join(
        (enter) => enter.append("text").attr("class", "rightHeader Header").on("click", _ScatterPlotMatrix.prototype.clickRowTitle.bind(thisMPlot)),
        (update) => update,
        (exit) => exit.remove()
      ).classed("input", function(rowIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isInput();
      }).classed("output", function(rowIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isOutput();
      }).classed("zAxis", function(colIndex) {
        return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]];
      }).attr("transform", function(rowIndex) {
        if (thisMPlot.rotateTitle) {
          return `translate(${thisMPlot.getSize() + 10}, ${tileHeight * (rowIndex + 0.3)})rotate(80)`;
        } else {
          return `translate(${thisMPlot.getSize() + 10}, ${tileHeight * (rowIndex + 0.5)})rotate(90)`;
        }
      }).each(function(rowIndex) {
        ScatterPlot.tspanColumnTitleBT(select_default2(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]]);
      }).attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
    }
    initBottomHeaders() {
      const thisMPlot = this;
      const mspGroup = select_default2(this.bindto + " .MultiPlot .mspGroup");
      const tileWidth = this.getSize() / thisMPlot.visibleDimCount;
      mspGroup.selectAll(".bottomHeader").data(range(this.visibleDimCount)).join(
        (enter) => enter.append("text").attr("class", "bottomHeader Header").on("click", _ScatterPlotMatrix.prototype.clickColTitle.bind(thisMPlot)),
        (update) => update,
        (exit) => exit.remove()
      ).classed("input", function(colIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isInput();
      }).classed("output", function(colIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isOutput();
      }).classed("zAxis", function(colIndex) {
        return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]];
      }).attr("transform", function(colIndex) {
        if (thisMPlot.rotateTitle) {
          return `translate(${tileWidth * (colIndex + 0.3)}, ${thisMPlot.getSize() + 55})rotate(-10)`;
        } else {
          return `translate(${tileWidth * (colIndex + 0.5)}, ${thisMPlot.getSize() + 45})`;
        }
      }).each(function(colIndex) {
        ScatterPlot.tspanColumnTitleTB(select_default2(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]);
      }).attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
    }
    initLeftHeaders() {
      const thisMPlot = this;
      const mspGroup = select_default2(this.bindto + " .MultiPlot .mspGroup");
      const tileHeight = this.getSize() / thisMPlot.visibleDimCount;
      mspGroup.selectAll(".leftHeader").data(range(this.visibleDimCount)).join(
        (enter) => enter.append("text").attr("class", "leftHeader Header").on("click", _ScatterPlotMatrix.prototype.clickRowTitle.bind(thisMPlot)),
        (update) => update,
        (exit) => exit.remove()
      ).classed("input", function(rowIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isInput();
      }).classed("output", function(rowIndex) {
        return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isOutput();
      }).classed("zAxis", function(colIndex) {
        return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]];
      }).attr("transform", function(rowIndex) {
        if (thisMPlot.rotateTitle) {
          return `translate(-55, ${tileHeight * (rowIndex + 0.8)})rotate(260)`;
        } else {
          return `translate(-45, ${tileHeight * (rowIndex + 0.5)})rotate(270)`;
        }
      }).each(function(rowIndex) {
        ScatterPlot.tspanColumnTitleBT(select_default2(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]]);
      }).attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
    }
    clickColTitle(_event, colIndex) {
      const dim = this.spData.dimensions[this.xStartingDimIndex + colIndex];
      if (this.zColumn !== null && this.zColumn.dim === dim) {
        this.setZAxis(null);
      } else {
        this.setZAxis(dim);
      }
      this.updateZAxisHeaders();
    }
    clickRowTitle(_event, rowIndex) {
      const dim = this.spData.dimensions[this.yStartingDimIndex + rowIndex];
      if (this.zColumn !== null && this.zColumn.dim === dim) {
        this.setZAxis(null);
      } else {
        this.setZAxis(dim);
      }
      this.updateZAxisHeaders();
    }
    updateZAxisHeaders() {
      const thisMPlot = this;
      const mspGroup = select_default2(this.bindto + " .MultiPlot .mspGroup");
      mspGroup.selectAll(".Header").classed("zAxis", function(colIndex) {
        return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]];
      });
    }
    xSubPlot(plot) {
      const tileWidth = this.getSize() / this.visibleDimCount;
      return plot.col * tileWidth;
    }
    ySubPlot(plot) {
      const tileHeight = this.getSize() / this.visibleDimCount;
      return plot.row * tileHeight;
    }
    //***********************************
    //********** About "ZAxis" **********
    //***********************************
    appendZAxisSelector() {
      const thisMPlot = this;
      selectAll_default2(this.bindto + " .ParamSelect").data([_ScatterPlotMatrix.zAxisClass]).append("select").on("change", function() {
        thisMPlot.updateZAxisFromGui();
      }).selectAll("option").data(this.spData.dimensions).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      if (this.zColumn !== null) {
        const paramIndex = this.spData.dimensions.indexOf(this.zColumn.dim);
        select_default2(this.bindto + " .ParamSelect > select").property("selectedIndex", paramIndex);
      }
    }
    initZAxisUsedCB() {
      select_default2(`#${this.id()}_zAxisUsed`).property("checked", this.zColumn !== null).on("change", _ScatterPlotMatrix.prototype.updateZAxisFromGui.bind(this));
    }
    updateZAxisFromGui() {
      if (select_default2(`#${this.id()}_zAxisUsed`).property("checked")) {
        const zAxisSelectNode = select_default2(this.bindto + " .ParamSelect.ZAxis>select").node();
        if (zAxisSelectNode) {
          this.setZAxis(this.spData.dimensions[zAxisSelectNode.selectedIndex]);
        }
      } else {
        this.setZAxis(null);
      }
    }
    //******************************************************
    //********** "Tooltip/Filter/Zoom" select box **********
    //******************************************************
    appendMouseModeSelect() {
      const thisMPlot = this;
      select_default2(this.bindto + " .mouseModeSelect").append("select").on("change", _ScatterPlotMatrix.prototype.mouseModeChange.bind(thisMPlot)).selectAll("option").data(SpConst.mouseModeList).enter().append("option").text(function(d) {
        return d.label;
      }).attr("value", function(d) {
        return d.key;
      });
      const modeIndex = SpConst.mouseModeList.findIndex((mode) => mode.key === this.mouseMode);
      select_default2(this.bindto + " .mouseModeSelect > select").property("selectedIndex", modeIndex);
    }
    mouseModeChange() {
      const mouseModeSelect = select_default2(this.bindto + " .mouseModeSelect > select").node();
      if (mouseModeSelect) {
        this.changeMouseMode(mouseModeSelect.options[mouseModeSelect.selectedIndex].value);
      }
    }
    changeMouseMode(mouseMode) {
      const modeIndex = SpConst.mouseModeList.findIndex((mode) => mode.key === mouseMode);
      if (modeIndex === -1) {
        console.error("Invalid mouse mode: " + mouseMode);
        return;
      }
      this.mouseMode = mouseMode;
      this.scatterPlotList.forEach((plot) => {
        plot.changeMouseMode(mouseMode);
      });
      this.diagPlotList.forEach((plot) => {
        plot.changeMouseMode(mouseMode);
      });
      select_default2(this.bindto + " .mouseModeSelect > select").property("selectedIndex", modeIndex);
      this.fixBrush();
    }
    fixBrush() {
      this.scatterPlotList.forEach((plot) => {
        plot.fixBrush();
      });
      this.diagPlotList.forEach((plot) => {
        plot.fixBrush();
      });
    }
    //***************************************************************
    //********** About "distribRep/Regression" check boxes **********
    //***************************************************************
    appendDistribRepSelect() {
      const thisMPlot = this;
      select_default2(this.bindto + " .distribRepSelect").append("select").on("change", function() {
        const rep = SpConst.distribRepList[this.selectedIndex];
        thisMPlot.setDistribType(rep.key === SpConst.histogramRep.key ? DistributionPlot.HISTO_REP : DistributionPlot.DENS_REP);
      }).selectAll("option").data(SpConst.distribRepList).enter().append("option").text(function(d) {
        return d.label;
      }).attr("value", function(d) {
        return d.key;
      });
      const histoRep = this.distribType & DistributionPlot.HISTO_REP ? SpConst.histogramRep.key : SpConst.densityRep.key;
      const repIndex = SpConst.distribRepList.findIndex((distribRep) => distribRep.key === histoRep);
      select_default2(this.bindto + " .distribRepSelect > select").property("selectedIndex", repIndex);
    }
    initRegressionCB() {
      const thisMPlot = this;
      select_default2(`#${this.id()}_linearRegr`).property("checked", (this.regressionType & RegressionPlot.LINEAR_REP) !== 0).on("change", function() {
        if (select_default2(this).property("checked")) {
          thisMPlot.setRegressionType(thisMPlot.regressionType | RegressionPlot.LINEAR_REP);
        } else {
          thisMPlot.setRegressionType(thisMPlot.regressionType ^ RegressionPlot.LINEAR_REP);
        }
      });
      select_default2(`#${this.id()}_loess`).property("checked", (this.regressionType & RegressionPlot.LOESS_REP) !== 0).on("change", function() {
        if (select_default2(this).property("checked")) {
          thisMPlot.setRegressionType(thisMPlot.regressionType | RegressionPlot.LOESS_REP);
        } else {
          thisMPlot.setRegressionType(thisMPlot.regressionType ^ RegressionPlot.LOESS_REP);
        }
      });
    }
    appendContCsSelect() {
      const thisMPlot = this;
      select_default2(this.bindto + " .contCsSelect").append("select").on("change", function() {
        const contCsKey = SpConst.CONTINUOUS_CS_IDS[this.selectedIndex];
        thisMPlot.setContinuousColorScale(contCsKey);
      }).selectAll("option").data(SpConst.CONTINUOUS_CS_IDS).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const contCsIndex = SpConst.CONTINUOUS_CS_IDS.indexOf(this.continuousCsId);
      select_default2(this.bindto + " .contCsSelect > select").property("selectedIndex", contCsIndex);
    }
    appendCatCsSelect() {
      const thisMPlot = this;
      select_default2(this.bindto + " .catCsSelect").append("select").on("change", function() {
        const catCsKey = SpConst.CATEGORIAL_CS_IDS[this.selectedIndex];
        thisMPlot.setCategoricalColorScale(catCsKey);
      }).selectAll("option").data(SpConst.CATEGORIAL_CS_IDS).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const catCsIndex = SpConst.CATEGORIAL_CS_IDS.indexOf(this.categoricalCsId);
      select_default2(this.bindto + " .catCsSelect > select").property("selectedIndex", catCsIndex);
    }
    appendCorrTypeSelect() {
      const thisMPlot = this;
      const corrPlotTypes = [CorrPlot.EMPTY_REP, CorrPlot.CIRCLES_REP, CorrPlot.TEXT_REP, CorrPlot.ABS_TEXT_REP];
      select_default2(this.bindto + " .corrTypeSelect").append("select").on("change", function() {
        const corrPlotType = corrPlotTypes[this.selectedIndex];
        thisMPlot.setCorrPlotType(corrPlotType);
      }).selectAll("option").data(corrPlotTypes).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const corrTypeIndex = corrPlotTypes.indexOf(this.corrPlotType);
      select_default2(this.bindto + " .corrTypeSelect > select").property("selectedIndex", corrTypeIndex);
    }
    appendCorrCsSelect() {
      const thisMPlot = this;
      select_default2(this.bindto + " .corrCsSelect").append("select").on("change", function() {
        const contCsKey = SpConst.CONTINUOUS_CS_IDS[this.selectedIndex];
        thisMPlot.setCorrPlotCS(contCsKey);
      }).selectAll("option").data(SpConst.CONTINUOUS_CS_IDS).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const corrCsIndex = SpConst.CONTINUOUS_CS_IDS.indexOf(this.corrPlotCsId);
      select_default2(this.bindto + " .corrCsSelect > select").property("selectedIndex", corrCsIndex);
    }
    //**************************************************
    //********** API (called by R htmlwidget) **********
    //**************************************************
    // eslint-disable-next-line max-lines-per-function
    setZAxis(dim, dontRedraw) {
      if (dim && !this.spData.columns[dim]) {
        console.error("setZAxis called with unknown dim:", dim);
        return;
      }
      const zAxisDim = this.zColumn === null ? null : this.zColumn.dim;
      if (dim === zAxisDim) {
        return;
      }
      const thisMPlot = this;
      this.zColumn = dim ? this.spData.columns[dim] : null;
      this.scatterPlotList.forEach((plot) => {
        const plotDim = plot.zColumn === null ? null : plot.zColumn.dim;
        if (plotDim !== dim) {
          plot.setZColumn(thisMPlot.zColumn);
          if (!dontRedraw) {
            plot.draw(ScatterPlot.Z_AXIS);
          }
        }
      });
      this.diagPlotList.forEach((plot) => {
        const plotDim = plot.zColumn === null ? null : plot.zColumn.dim;
        if (plotDim !== dim) {
          plot.setZColumn(thisMPlot.zColumn);
          if (!dontRedraw) {
            plot.draw(ScatterPlot.Z_AXIS);
          }
        }
      });
      this.corrPlotList.forEach((plot) => {
        const plotDim = plot.zColumn === null ? null : plot.zColumn.dim;
        if (plotDim !== dim) {
          plot.setZColumn(thisMPlot.zColumn);
          if (!dontRedraw) {
            plot.draw(ScatterPlot.Z_AXIS);
          }
        }
      });
      this.spData.dispatch.call(_ScatterPlotMatrix.PLOT_EVENT, void 0, {
        type: _ScatterPlotMatrix.ZAXIS_EVENT,
        value: dim
      });
      if (select_default2(this.bindto + " .mspDiv.withWidgets").size() !== 0) {
        select_default2(`#${this.id()}_zAxisUsed`).property("checked", this.zColumn !== null);
        if (this.zColumn !== null) {
          const zAxisSelectNode = select_default2(this.bindto + " .ParamSelect.ZAxis>select").node();
          if (zAxisSelectNode) {
            const selectedIndex = this.spData.dimensions.indexOf(this.zColumn.dim);
            if (selectedIndex === -1) {
              console.warn("Dim of Z axis not found => selectedIndex cannot be updated");
            } else {
              zAxisSelectNode.selectedIndex = selectedIndex;
            }
          }
        }
      }
    }
    setDistribType(distribType) {
      if (distribType & (DistributionPlot.HISTO_REP | DistributionPlot.DENS_REP)) {
        this.distribType = distribType;
        this.scatterPlotList.forEach((plot) => {
          plot.distribRepChange(distribType);
        });
        this.diagPlotList.forEach((plot) => {
          plot.distribRepChange(distribType);
        });
      } else {
        console.error("Invalid distribution type code: " + distribType);
      }
    }
    setRegressionType(regressionType) {
      if (regressionType === 0 || regressionType & (RegressionPlot.LINEAR_REP | RegressionPlot.LOESS_REP)) {
        this.regressionType = regressionType;
        this.scatterPlotList.forEach((plot) => {
          plot.regressionRepChange(regressionType);
        });
      } else {
        console.error("Invalid regression type code: " + regressionType);
      }
    }
    setContinuousColorScale(continuousCsId) {
      if (SpConst.CONTINUOUS_CS_IDS.includes(continuousCsId)) {
        this.continuousCsId = continuousCsId;
        this.scatterPlotList.forEach((plot) => {
          plot.continuousCsId = continuousCsId;
        });
        this.diagPlotList.forEach((plot) => {
          plot.continuousCsId = continuousCsId;
        });
        this.updatePlots(ScatterPlot.PALETTE);
      } else {
        console.error("Unknown continuous color scale: " + continuousCsId);
      }
    }
    setCategoricalColorScale(categoricalCsId) {
      if (SpConst.CATEGORIAL_CS_IDS.includes(categoricalCsId)) {
        this.categoricalCsId = categoricalCsId;
        this.scatterPlotList.forEach((plot) => {
          plot.categoricalCsId = categoricalCsId;
        });
        this.diagPlotList.forEach((plot) => {
          plot.categoricalCsId = categoricalCsId;
        });
        this.corrPlotList.forEach((plot) => {
          plot.categoricalCsId = categoricalCsId;
        });
        this.updatePlots(ScatterPlot.PALETTE);
      } else {
        console.error("Unknown categorical color scale: " + categoricalCsId);
      }
    }
    setCorrPlotType(corrPlotType) {
      if ([CorrPlot.EMPTY_REP, CorrPlot.CIRCLES_REP, CorrPlot.TEXT_REP, CorrPlot.ABS_TEXT_REP].includes(corrPlotType)) {
        this.corrPlotType = corrPlotType;
        this.corrPlotList.forEach((plot) => {
          plot.repType = corrPlotType;
          plot.draw(ScatterPlot.SHAPE);
        });
      } else {
        console.error("Unknown correlation plot type: " + corrPlotType);
      }
    }
    setCorrPlotCS(corrPlotCsId) {
      if (SpConst.CONTINUOUS_CS_IDS.includes(corrPlotCsId)) {
        this.corrPlotCsId = corrPlotCsId;
        this.corrPlotList.forEach((plot) => {
          plot.corrCsId = corrPlotCsId;
        });
        this.updatePlots(ScatterPlot.PALETTE);
      } else {
        console.error("Unknown correlation color scale: " + corrPlotCsId);
      }
    }
    setCutoffs(spCutoffsList) {
      this.spData.setCutoffs(spCutoffsList);
      this.fixBrush();
    }
    highlightPoint(pointIndex) {
      if (this.scatterPlotList.length > 0) {
        this.spData.changeHlPoint(pointIndex, this.scatterPlotList[0], false);
      }
    }
    setKeptColumns(keptColumns) {
      const thisPlot = this;
      if (Array.isArray(keptColumns)) {
        this.spData.dimensions = Object.keys(this.spData.sampleData[0]).filter(
          (_dim, i) => keptColumns[i]
        );
      } else {
        this.spData.dimensions = Object.keys(this.spData.sampleData[0]).filter(function(dim) {
          let toKeep = thisPlot.spData.dimensions.includes(dim);
          if (typeof keptColumns[dim] !== "undefined") {
            toKeep = keptColumns[dim];
          }
          return toKeep;
        });
      }
      this.adjustVisibleDimensions();
      this.tilesNumberChanged();
      this.xBrushSlider.update();
      this.yBrushSlider.update();
    }
    getPlotConfig() {
      const allDimensions = Object.keys(this.spData.sampleData[0]);
      const controlWidgets = select_default2(this.bindto + " .mspDiv").classed("withWidgets");
      const categorical = allDimensions.map(
        (dim) => this.spData.columns[dim] ? this.spData.columns[dim].categories : null
      );
      const inputColumns = allDimensions.map((dim) => this.spData.columns[dim] && this.spData.columns[dim].ioType === Column.INPUT);
      const keptColumns = allDimensions.map((dim) => this.spData.dimensions.includes(dim));
      const zAxisDim = this.zColumn === null ? null : this.zColumn.dim;
      const columnLabels = allDimensions.map(
        (dim) => this.spData.columns[dim] && this.spData.columns[dim].label ? this.spData.columns[dim].label : dim
      );
      return {
        data: [],
        rowLabels: this.spData.rowLabels,
        categorical,
        inputColumns,
        keptColumns,
        cutoffs: this.spData.getXYCutoffs(),
        zAxisDim,
        distribType: this.distribType,
        regressionType: this.regressionType,
        corrPlotType: this.corrPlotType,
        corrPlotCS: this.corrPlotCsId,
        rotateTitle: this.rotateTitle,
        columnLabels,
        continuousCS: this.continuousCsId,
        categoricalCS: this.categoricalCsId,
        mouseMode: this.mouseMode,
        controlWidgets,
        cssRules: this.style.cssRules,
        plotProperties: this.style.plotProperties,
        slidersPosition: {
          dimCount: this.visibleDimCount,
          xStartingDimIndex: this.xStartingDimIndex,
          yStartingDimIndex: this.yStartingDimIndex
        }
      };
    }
  };

  // src/Typescript/spmHtmlwidget.ts
  HTMLWidgets.widget({
    name: "scatterPlotMatrix",
    type: "output",
    // eslint-disable-next-line max-lines-per-function
    factory: function(el, width, height) {
      function js2RIndex(index) {
        return typeof index === "number" ? index + 1 : index;
      }
      function r2JsIndex(index) {
        return typeof index === "number" ? index - 1 : index;
      }
      const scatterPlotMatrix = new ScatterPlotMatrix(el.id, width, height);
      return {
        // eslint-disable-next-line max-lines-per-function
        renderValue: function(config) {
          document.getElementById(scatterPlotMatrix.id()).widget = this;
          const controlWidgets = config.controlWidgets === null ? !HTMLWidgets.shinyMode : config.controlWidgets;
          const slidersPosition = config.slidersPosition ? {} : null;
          if (config.slidersPosition) {
            if (typeof config.slidersPosition.dimCount === "number") {
              slidersPosition.dimCount = config.slidersPosition.dimCount;
            }
            if (typeof config.slidersPosition.xStartingDimIndex === "number") {
              slidersPosition.xStartingDimIndex = r2JsIndex(config.slidersPosition.xStartingDimIndex);
            }
            if (typeof config.slidersPosition.yStartingDimIndex === "number") {
              slidersPosition.yStartingDimIndex = r2JsIndex(config.slidersPosition.yStartingDimIndex);
            }
          }
          scatterPlotMatrix.generate({
            data: HTMLWidgets.dataframeToD3(config.data),
            rowLabels: config.rowLabels,
            categorical: config.categorical,
            inputColumns: config.inputColumns,
            cutoffs: config.cutoffs,
            keptColumns: config.keptColumns,
            zAxisDim: config.zAxisDim,
            distribType: config.distribType,
            regressionType: config.regressionType,
            corrPlotType: config.corrPlotType,
            corrPlotCS: config.corrPlotCS,
            rotateTitle: config.rotateTitle,
            columnLabels: config.columnLabels,
            continuousCS: config.continuousCS,
            categoricalCS: config.categoricalCS,
            mouseMode: config.mouseMode,
            controlWidgets,
            cssRules: config.cssRules,
            plotProperties: config.plotProperties,
            slidersPosition
          });
          if (HTMLWidgets.shinyMode) {
            ["setDistribType", "setRegressionType", "setCorrPlotType", "setCorrPlotCS", "setContinuousColorScale", "setCategoricalColorScale", "setCutoffs", "highlightPoint", "setKeptColumns", "changeMouseMode", "setZAxis", "getPlotConfig"].forEach((func) => {
              Shiny.addCustomMessageHandler("scatterPlotMatrix:" + func, function(message) {
                const elem = document.getElementById(message.id);
                if (elem) {
                  elem.widget[func](message);
                }
              });
            });
            if (config.eventInputId !== null) {
              scatterPlotMatrix.on(ScatterPlotMatrix.PLOT_EVENT, function(event) {
                if (event.type === ScatterPlotMatrix.POINT_CLICK_EVENT || event.type === ScatterPlotMatrix.HL_POINT_EVENT) {
                  event.value.pointIndex = js2RIndex(event.value.pointIndex);
                }
                Shiny.setInputValue(config.eventInputId, event, { priority: "event" });
              });
            }
          }
        },
        // End 'renderValue'
        setDistribType: function(params) {
          scatterPlotMatrix.setDistribType(params.distribType);
        },
        setRegressionType: function(params) {
          scatterPlotMatrix.setRegressionType(params.regressionType);
        },
        setCorrPlotType: function(params) {
          scatterPlotMatrix.setCorrPlotType(params.corrPlotType);
        },
        setCorrPlotCS: function(params) {
          scatterPlotMatrix.setCorrPlotCS(params.corrPlotCsId);
        },
        setContinuousColorScale: function(params) {
          scatterPlotMatrix.setContinuousColorScale(params.continuousCsId);
        },
        setCategoricalColorScale: function(params) {
          scatterPlotMatrix.setCategoricalColorScale(params.categoricalCsId);
        },
        setCutoffs: function(params) {
          scatterPlotMatrix.setCutoffs(params.cutoffs);
        },
        highlightPoint: function(params) {
          scatterPlotMatrix.highlightPoint(r2JsIndex(params.pointIndex), params.newValues);
        },
        setKeptColumns: function(params) {
          scatterPlotMatrix.setKeptColumns(params.keptColumns);
        },
        changeMouseMode: function(params) {
          scatterPlotMatrix.changeMouseMode(params.interactionType);
        },
        setZAxis: function(params) {
          scatterPlotMatrix.setZAxis(params.dim);
        },
        getPlotConfig: function(params) {
          if (HTMLWidgets.shinyMode) {
            const plotConfig = scatterPlotMatrix.getPlotConfig();
            if (plotConfig.slidersPosition && typeof plotConfig.slidersPosition.xStartingDimIndex === "number") {
              plotConfig.slidersPosition.xStartingDimIndex = js2RIndex(plotConfig.slidersPosition.xStartingDimIndex);
            }
            if (plotConfig.slidersPosition && typeof plotConfig.slidersPosition.yStartingDimIndex === "number") {
              plotConfig.slidersPosition.yStartingDimIndex = js2RIndex(plotConfig.slidersPosition.yStartingDimIndex);
            }
            Shiny.setInputValue(params.configInputId, plotConfig, { priority: "event" });
          }
        },
        resize: function(newWidth, newHeight) {
          scatterPlotMatrix.resize(newWidth, newHeight);
        }
      };
    }
    // End 'factory'
  });
})();
