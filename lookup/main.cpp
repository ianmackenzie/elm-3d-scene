#include <cmath>
#include <iostream>
#include <future>
#include <thread>
#include <deque>

#include "lodepng.h"

#ifndef PI
#define PI 3.1415926535897932384626433832795
#endif

struct Vec3
{
    double x;
    double y;
    double z;
};

Vec3 operator+(const Vec3& v1, const Vec3& v2) {
    return {v1.x + v2.x, v1.y + v2.y, v1.z + v2.z};
}

double dot(const Vec3& v1, const Vec3& v2) {
    return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
}

Vec3 normalize(const Vec3& vec) {
    double magnitude = std::sqrt(dot(vec, vec));
    return {vec.x / magnitude, vec.y / magnitude, vec.z / magnitude};
}

double quotient(double alphaSquared, double k, double dotNHSquared, double dotNL, double dotNV) {
    double a = dotNHSquared * (alphaSquared - 1.0) + 1.0;
    double b = dotNL * (1.0 - k) + k;
    double c = dotNV * (1.0 - k) + k;
    return alphaSquared / (4.0 * PI * a * a * b * c);
}

double schlickTerm(double dotVH) {
    double base = 1.0 - dotVH;
    double baseSquared = base * base;
    return baseSquared * baseSquared * base;
}

double spline(double p0, double p1, double p2, double t) {
    double p01 = p0 + t * (p1 - p0);
    double p12 = p1 + t * (p2 - p1);
    return p01 + t * (p12 - p01);
}

double interpolate(double x, double x0) {
    if (x < x0) {
        return spline(0.0, x0, x0, x / x0);
    } else {
        return spline(x0, x0, 1.0, (x - x0) / (1.0 - x0));
    }
}

double interpolateDerivative(double x, double x0) {
    if (x < x0) {
        return 2.0 - 2.0 * x / x0;
    } else {
        return 2.0 * (x - x0) / (1.0 - x0);
    }
}

int main (int argc, char** argv) {
    int width = 256;
    int height = 256;
    std::vector<unsigned char> imageData(4 * width * height);
    std::deque<std::future<void>> futures;

    std::cout << "Will save to " << argv[1] << std::endl;
    int receivedIndex = 0;
    for (int imageY = 0; imageY < height; ++imageY) {
        futures.emplace_back(
            std::async(
                std::launch::async,
                [&imageData, width, height, imageY] () {
                    for (int imageX = 0; imageX < width; ++imageX) {
                        int nPhi = 4000;
                        int nTheta = 2000;

                        double dotNV = (imageX + 0.5) / width;
                        double roughness = (imageY + 0.5) / height;

                        double alpha = roughness * roughness;
                        double alphaSquared = alpha * alpha;
                        double k = alpha / 2;
                        Vec3 normalDirection = {0.0, 0.0, 1.0};
                        Vec3 viewDirection = {-std::sqrt(1.0 - dotNV * dotNV), 0.0, dotNV};

                        double t0 = std::acos(dotNV) / (PI / 2.0);

                        double scale = 0.0;
                        double offset = 0.0;

                        for (int i = 0; i < nPhi; ++i) {
                            double t = (i + 0.5) / nPhi;
                            double phi = interpolate(t, t0) * PI / 2.0;
                            double dPhi = interpolateDerivative(t, t0) * (PI / 2.0) / nPhi;

                            double sinPhi = std::sin(phi);

                            for (int j = 0; j < nTheta; ++j) {
                                double u = (j + 0.5) / nTheta;
                                double uSquared = u * u;
                                double theta = u * uSquared * PI;
                                double dTheta = 3 * uSquared * PI / nTheta;

                                double dA = 2.0 * sinPhi * dPhi * dTheta;

                                Vec3 lightDirection = {sinPhi * std::cos(theta), sinPhi * std::sin(theta), std::cos(phi)};
                                Vec3 halfDirection = normalize(viewDirection + lightDirection);

                                double dotVH = dot(viewDirection, halfDirection);
                                double dotNH = dot(normalDirection, halfDirection);
                                double dotNL = dot(normalDirection, lightDirection);
                                double dotNHSquared = dotNH * dotNH;

                                double schlickValue = schlickTerm(dotVH);

                                double quotientValue = quotient(alphaSquared, k, dotNHSquared, dotNL, dotNV);

                                double commonTerm = quotientValue * dotNL * dA;

                                scale += commonTerm * (1.0 - schlickValue);
                                offset += commonTerm * schlickValue;
                            }
                        }

                        int startIndex = imageX * 4 + width * 4 * (height - 1 - imageY);
                        imageData[startIndex] = std::round(255 * scale);
                        imageData[startIndex + 1] = std::round(255 * offset);
                        imageData[startIndex + 2] = 0;
                        imageData[startIndex + 3] = 255;
                    }
                }
            )
        );

        while (futures.size() > 3) {
            futures.front().wait();
            futures.pop_front();
            std::cout << "Received future " << receivedIndex << std::endl;
            ++receivedIndex;
        }
    }

    for (const auto& future: futures) {
        future.wait();
        std::cout << "Received future " << receivedIndex << std::endl;
        ++receivedIndex;
    }

    std::cout << "Encoding..." << std::endl;
    lodepng::encode(argv[1], imageData.data(), width, height);
    return 0;
}
